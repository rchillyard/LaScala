package com.phasmid.laScala.tree

import com.phasmid.laScala.Kleenean
import com.phasmid.laScala.fp.{FP, HasKey}
import org.scalatest.{FlatSpec, Matchers}

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

        val tree = ParentChildTree.populateParentChildTree(as map Value[String, AccountRecord])
        println(tree)
//        tree.size shouldBe 100
        println(tree.render())
//        tree.depth shouldBe 2
        val ns = tree.nodeIterator(true)
        val lt: Int=>Boolean = {_<0}
        val eq: Int=>Boolean = {_==0}
        def compareWithDate(f: Int=>Boolean)(d: AccountDate)(n: Node[NodeType]): Boolean = n.get match {
          case Some(Value(v)) => f(v.date.compare(d))
          case _ => false
        }

        val onDate = compareWithDate(eq) _
        val beforeDate = compareWithDate(lt) _
        tree.find(onDate(AccountDate(2014, 9, 30))) should matchPattern { case Some(n) => }
        tree.filter(beforeDate(AccountDate(2014, 9, 30))).size shouldBe 52

        // TODO recreate this test
        val indexedTree = KVTree.createIndexedTree(tree)
        indexedTree.nodeIterator(true).size shouldBe 100
        val mptt = MPTT (indexedTree)
        mptt.index.size shouldBe 34
        println (mptt)

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
    * @param values
    * @param treeBuilder
    * @param leafBuilder
    * @tparam V
    * @return
    */
  def populateParentChildTree[V](values: Seq[Value[String, V]])(implicit ev: HasParent[String,Value[String,V]], treeBuilder: TreeBuilder[Value[String, V]], leafBuilder: LeafBuilder[Value[String, V]], valueBuilder: ValueBuilder[String,V]): TreeLike[Value[String, V]] =
  values match {
    case h :: t =>
      // TODO let's do this as a proper tail-recursion without using a var!
      var tree: KVTree[String,V] = implicitly[TreeBuilder[Value[String, V]]].buildTree(implicitly[LeafBuilder[Value[String, V]]].buildLeaf(h), Empty).asInstanceOf[KVTree[String,V]]
      for (w <- t) {
        tree.attachNode(tree, w) match {
          case Success(x) => tree = x.asInstanceOf[KVTree[String, V]]
          case Failure(x) => throw x
        }
      }
      tree
  }
}