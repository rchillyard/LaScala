package com.phasmid.laScala.tree

import com.phasmid.laScala.Kleenean
import com.phasmid.laScala.fp.{FP, HasKey}
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
class FunctionalTest extends FlatSpec with Matchers {

  type NodeType = Value[AccountRecord]

  behavior of "Recursive account lookup"
  it should "work for miniSampleTree.txt" in {
    //    Spy.spying = true
    val uo = Option(getClass.getResource("miniSampleTree.txt"))
    uo should matchPattern { case Some(_) => }
    val so = uo map ( _.openStream )
    val wsso = for (s <- so) yield for (l <- Source.fromInputStream(s).getLines) yield for (w <- l.split("""\|""")) yield w
    val aoso = for (wss <- wsso) yield for(ws <- wss) yield AccountRecord.parse(ws(2), ws(0), ws(1))

    // CONSIDER Now, we flatten the options, resulting in None if there were any problems at all. May want to change the behavior of this later
    val aso = (for (aos <- aoso) yield FP.sequence(aos.toSeq)).flatten
    aso match {
      case Some(as) =>
        import AccountRecord._
        implicit object GeneralKVTreeBuilderNodeType extends GeneralKVTreeBuilder[AccountRecord]
        implicit object GeneralKVLeafBuilderValueNodeType extends GeneralKVLeafBuilder[AccountRecord]
        implicit object ValueBuilderNodeType extends ValueBuilder[AccountRecord] {
          // TODO fix this totally arbitrary value for date
          def buildValue(k: String): Value[AccountRecord] = Value(AccountRecord(k, AccountDate(1900, 1, 1), "root"))
        }
        implicit object NodeTypeParent extends HasParent[NodeType] {
          def getParentKey(t: NodeType): Option[String] = Some(t.value.parent)
          def createParent(t: NodeType): Option[TreeLike[NodeType]] = {
            val treeBuilder = implicitly[TreeBuilder[NodeType]]
            //            val leafBuilder = implicitly[LeafBuilder[NodeType]]
            val vo = for (k <- getParentKey(t)) yield implicitly[ValueBuilder[AccountRecord]].buildValue(k)
            for (_ <- vo) yield treeBuilder.buildTree(vo, Seq())
          }
        }
        implicit object NodeTypeNodeParent extends NodeParent[NodeType] {
          // XXX see comment on GeneralNodeParent
          def isParent(parent: Node[NodeType], child: Node[NodeType]): Boolean = parent match {
            case Branch(ns) => ns.contains(child)
            case _ => false
          }
        }

        ParentChildTree.populateParentChildTree(as map Value[AccountRecord]) match {
          case Success(tree) =>
            println(tree)
            tree.size shouldBe 9
            println(tree.render())
            tree.depth shouldBe 3
            val ns = tree.nodeIterator(true)
            println(ns.toList)
            val lt: Int => Boolean = _ < 0
            val eq: Int => Boolean = _ == 0
            def compareWithDate(f: Int => Boolean)(d: AccountDate)(n: Node[NodeType]): Boolean = n.get match {
              case Some(Value(AccountRecord("root",_,_))) => false
              case Some(Value(v)) => f(v.date.compare(d))
              case _ => false
            }

            val onDate = compareWithDate(eq) _
            val beforeDate = compareWithDate(lt) _
            tree.find(onDate(AccountDate(2014, 9, 30))) should matchPattern { case Some(_) => }
            tree.filter(beforeDate(AccountDate(2014, 9, 30))).size shouldBe 5

            // TODO recreate this test
            val indexedTree = KVTree.createIndexedTree(tree)
            indexedTree.nodeIterator(true).size shouldBe 9
            val mptt = MPTT (indexedTree)
            mptt.index.size shouldBe 6
            println (mptt)
          case Failure(x) =>
            fail(s"Exception thrown populating tree",x)
        }

      case None => System.err.println("unable to yield a complete hierarchy")
    }
  }

  ignore should "work for sampleTree.txt" in {
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
        implicit object GeneralKVTreeBuilderNodeType extends GeneralKVTreeBuilder[AccountRecord]
        implicit object GeneralKVLeafBuilderValueNodeType extends GeneralKVLeafBuilder[AccountRecord]
        implicit object ValueBuilderNodeType extends ValueBuilder[AccountRecord] {
          def buildValue(k: String): Value[AccountRecord] = Value(AccountRecord(k, null, null))
        }
        implicit object NodeTypeParent extends HasParent[NodeType] {
          def getParentKey(t: NodeType): Option[String] = Some(t.value.parent)
          def createParent(t: NodeType): Option[TreeLike[NodeType]] = {
            val treeBuilder = implicitly[TreeBuilder[NodeType]]
            val leafBuilder = implicitly[LeafBuilder[NodeType]]
            val vo = for (k <- getParentKey(t)) yield implicitly[ValueBuilder[AccountRecord]].buildValue(k)
            for (v <- vo) yield treeBuilder.buildTree(None,Seq(leafBuilder.buildLeaf(v)))
          }
        }
        implicit object NodeTypeNodeParent extends NodeParent[NodeType] {
          // XXX see comment on GeneralNodeParent
          def isParent(parent: Node[NodeType], child: Node[NodeType]): Boolean = parent match {
            case Branch(ns) => ns.contains(child)
            case _ => false
          }
        }

        ParentChildTree.populateParentChildTree(as map Value[AccountRecord]) match {
          case Success(tree) =>
            println(tree)
            tree.size shouldBe 100
            println(tree.render())
            tree.depth shouldBe 2
            val ns = tree.nodeIterator(true)
            println(ns)
            val lt: Int => Boolean = _ < 0
            val eq: Int => Boolean = _ == 0
            def compareWithDate(f: Int => Boolean)(d: AccountDate)(n: Node[NodeType]): Boolean = n.get match {
              case Some(Value(AccountRecord("root",_,_))) => false
              case Some(Value(v)) => f(v.date.compare(d))
              case _ => false
            }

            val onDate = compareWithDate(eq) _
            val beforeDate = compareWithDate(lt) _
            tree.find(onDate(AccountDate(2014, 9, 30))) should matchPattern { case Some(_) => }
            tree.filter(beforeDate(AccountDate(2014, 9, 30))).size shouldBe 52

            // TODO recreate this test
            val indexedTree = KVTree.createIndexedTree(tree)
            indexedTree.nodeIterator(true).size shouldBe 101
            val mptt = MPTT (indexedTree)
            mptt.index.size shouldBe 35
            println (mptt)
          case Failure(x) =>
            fail(s"Exception thrown populating tree",x)
        }

      case None => System.err.println("unable to yield a complete hierarchy")
    }
  }
}

object FunctionalTest {
  object NodeType {

    trait NodeTypeParent extends HasParent[Value[AccountRecord]] {
      def createParent(t: Value[AccountRecord]): Option[Node[Value[AccountRecord]]] = None

      def getParentKey(v: Value[AccountRecord]): Option[String] = Some(v.value.parent)
    }
    implicit object HasParentNodeType extends NodeTypeParent
  }
}

object ParentChildTree {
  /**
    * This implementation of populateGeneralKVTree takes a sequence of Values, each of which specifies the parent as well as the attributes.
    *
    * NOTE: this implementation relies on the fact that parent nodes, if they have values at all, will be mentioned BEFORE any of their children
    *
    * @param values      the values to become nodes in the resulting tree
    * @param treeBuilder the tree builder
    * @param leafBuilder the leaf builder
    * @tparam V the underlying type of the Values
    * @return the newly created tree
    */
  def populateParentChildTree[V](values: Seq[Value[V]])(implicit ev1: HasParent[Value[V]], ev2: NodeParent[Value[V]], treeBuilder: TreeBuilder[Value[V]], leafBuilder: LeafBuilder[Value[V]], valueBuilder: ValueBuilder[V]): Try[TreeLike[Value[V]]] =
  {
    val root = valueBuilder.buildValue("root")
    val ty = Try(implicitly[TreeBuilder[Value[V]]].buildTree(Some(root), Seq()).asInstanceOf[KVTree[V]])
      @tailrec
      def inner(result: Try[TreeLike[Value[V]]], values: List[Value[V]]): Try[TreeLike[Value[V]]] = values match {
        case Nil => result
        case y :: z => inner(for (t <- result; u = t :+ y) yield u, z)
      }
      inner(ty, values.toList)
  }
}