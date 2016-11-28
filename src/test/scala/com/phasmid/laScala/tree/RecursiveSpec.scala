package com.phasmid.laScala.tree

import com.phasmid.laScala.Kleenean
import com.phasmid.laScala.fp.{FP, HasKey, Spy}
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.io.Source
import scala.util._

case class AccountRecord(account: String, date: AccountDate, parent: String)

case class AccountDate(year: Int, month: Int, day: Int) extends Ordering[AccountDate] {
  def compare(other: AccountDate): Int = compare(this,other)
  def compare(x: AccountDate, y: AccountDate): Int =
    Kleenean(Kleenean(x.year.compareTo(y.year))() orElse Kleenean(x.month.compareTo(y.month))() orElse Kleenean(x.day.compareTo(y.day))()).toInt
}

object AccountRecord {
  def parse(account: String, date: String, parent: String): Option[AccountRecord] = {
    val nodeR = """([A-Z\d-]{20})""".r
    val accountR = """([A-Z\d-]{4,20})""".r
    val dateR = """(\d{4})-(\d{2})-(\d{2})""".r
    val p = parent match {
      case nodeR(x) => Success(x)
      case _ => Failure(TreeException(s"parent node incorrectly formatted: $parent"))
    }
    val a = account match {
      case accountR(x) => Success(x)
      case _ => Failure(TreeException(s"account incorrectly formatted: $account"))
    }
    val d = date match {
      case dateR(y,m,n) => AccountDate.parse(y,m,n)
      case _ => Failure(TreeException(s"account date incorrectly formatted: $date"))
    }
    FP.toOption(FP.map3(a,d,p)(apply))
  }

  abstract class HasKeyAccountRecord extends HasKey[AccountRecord] {
    type K = String
    def getKey(x: AccountRecord): K = x.account
  }
  implicit object HasKeyAccountRecord extends HasKeyAccountRecord
}

object AccountDate {
  def parse(y: String, m: String, d: String): Try[AccountDate] = FP.map3(Try(y.toInt), Try(m.toInt), Try(d.toInt))(apply)
}

//object DatePredicates {
//  val lt: Int=>Boolean = {_<0}
//  val eq: Int=>Boolean = {_==0}
//  def compareNodeAttribute[K,V,X : Ordering](g: V=>X)(f: Int=>Boolean)(d: X)(n: Node[Value[K,V]]): Boolean = n.get match {
//    case Some(Value(v)) => f(implicitly[Ordering[X]].compare(g(v),d))
//    case _ => false
//  }
//
//  val onDate = compareWithDate(eq) _
//  val beforeDate = compareWithDate(lt) _
//
//}
/**
  * Created by scalaprof on 10/19/16.
  */
class RecursiveSpec extends FlatSpec with Matchers {


  
  type NodeType = Value[String,AccountRecord]


  behavior of "Recursive account lookup"
  it should "work" in {

    val uo = Option(getClass.getResource("sampleTree.txt"))
    uo should matchPattern { case Some(_) => }
    val so = uo map ( _.openStream )
    val wsso = for (s <- so) yield for (l <- Source.fromInputStream(s).getLines) yield for (w <- l.split("""\|""")) yield w
    val aoso = for (wss <- wsso) yield for(ws <- wss) yield AccountRecord.parse(ws(7), ws(5), ws(6))

    // CONSIDER Now, we flatten the options, resulting in None if there were any problems at all. May want to change the behavior of this later
    val aso = (for (aos <- aoso) yield FP.sequence(aos.toSeq)).flatten
    aso match {
      case Some(as) =>
        import AccountRecord._
        implicit object GeneralKVTreeBuilderNodeType extends GeneralKVTreeBuilder[String,AccountRecord]
        implicit object GeneralKVLeafBuilderValueNodeType extends GeneralKVLeafBuilder[String,AccountRecord]
        implicit object NodeTypeParent extends HasParent[String,NodeType] {
         def getParent(t: NodeType): String = t.value.parent
        }
        implicit object ValueBuilderNodeType extends ValueBuilder[String,AccountRecord] {
          def buildValue(k: String): Value[String, AccountRecord] = Value(AccountRecord(k,null,null))
        }
        implicit object NodeTypeNodeParent extends NodeParent[NodeType] {
          // XXX see comment on GeneralNodeParent
          def isParent(parent: Node[NodeType], child: Node[NodeType]): Boolean = parent match {
            case Branch(ns) => ns.contains(child)
            case _ => false
          }
        }

        ParentChildTree.populateParentChildTree(as map Value[String, AccountRecord]) match {
          case Success(tree) =>
            println(tree)
            //        tree.size shouldBe 100
            println(tree.render())
            //        tree.depth shouldBe 2
            val ns = tree.nodeIterator(true)
            val lt: Int => Boolean = _ < 0
            val eq: Int => Boolean = _ == 0
            def compareWithDate(f: Int => Boolean)(d: AccountDate)(n: Node[NodeType]): Boolean = n.get match {
              case Some(Value(AccountRecord("root",_,_))) => false
              case Some(Value(v)) => println(s"compareWithDate: v=$v, d=$d"); f(v.date.compare(d))
              case _ => false
            }

            val onDate = compareWithDate(eq) _
            val beforeDate = compareWithDate(lt) _;
            tree.find(onDate(AccountDate(2014, 9, 30))) should matchPattern {
              case Some(n) =>
            }
            tree.filter(beforeDate(AccountDate(2014, 9, 30))).size shouldBe 52

            // TODO recreate this test
            val indexedTree = KVTree.createIndexedTree(tree)
            indexedTree.nodeIterator(true).size shouldBe 101
            val mptt = MPTT (indexedTree)
            mptt.index.size shouldBe 35
            println (mptt)
          case Failure(x) => System.err.println(s"exception: ${x.getLocalizedMessage}")
        }

      case None => System.err.println("unable to yield a complete hierarchy")
    }
  }
}

object RecursiveSpec {
  object NodeType {
    abstract class HasParentNodeType extends HasParent[String,Value[String,AccountRecord]] {
      def getParent(v: Value[String,AccountRecord]): String = v.value.parent
    }
    implicit object HasParentNodeType extends HasParentNodeType
  }
}

object ParentChildTree {
  /**
    * This implementation of populateGeneralKVTree takes a sequence of Values, each of which specifies the parent as well as the attributes.
    *
    * NOTE: this implementation relies on the fact that parent nodes, if they have values at all, will be mentioned BEFORE any of their children
    * @param values
    * @param treeBuilder
    * @param leafBuilder
    * @tparam V
    * @return
    */
  def populateParentChildTree[V](values: Seq[Value[String, V]])(implicit ev1: HasParent[String, Value[String, V]], ev2: NodeParent[Value[String, V]], treeBuilder: TreeBuilder[Value[String, V]], leafBuilder: LeafBuilder[Value[String, V]], valueBuilder: ValueBuilder[String, V]): Try[TreeLike[Value[String, V]]] =
  {
    val root = valueBuilder.buildValue("root")
      val ty = Try(implicitly[TreeBuilder[Value[String, V]]].buildTree(Some(root), Seq()).asInstanceOf[KVTree[String, V]])
      println(s"initial tree: $ty")
      @tailrec
      def inner(result: Try[TreeLike[Value[String, V]]], values: List[Value[String, V]]): Try[TreeLike[Value[String, V]]] = values match {
        case Nil => result
        case y :: z => println(s"attach $y to tree"); inner(for (t <- result; u = t :+ Some(y)) yield u, z)
      }
      inner(ty, values.toList)
  }
}