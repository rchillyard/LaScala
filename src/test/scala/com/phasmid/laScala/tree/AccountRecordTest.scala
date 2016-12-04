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

trait BuildAccountRecord {
  def createAccountRecord(ws: Array[String]): Option[AccountRecord]
}
abstract class AbstractTestDetails(val resourceName: String) extends BuildAccountRecord

/**
  * Created by scalaprof on 10/19/16.
  */
class AccountRecordTest extends FlatSpec with Matchers {

  behavior of "Recursive account lookup"
  it should "work for miniSampleTree.txt" in {
    case object TestDetailsMiniSample extends AbstractTestDetails("miniSampleTree.txt") {
      def createAccountRecord(ws: Array[String]): Option[AccountRecord] = AccountRecord.parse(ws(2), ws(0), ws(1))
    }
    checkTreeFromResource(TestDetailsMiniSample, 9, 3, 5, 9, 6)
  }
  // XXX we ignore this because I have not committed the sampleTree.txt file to the repository.
  it should "work for sampleTree.txt" in {
    case object TestDetailsSample extends AbstractTestDetails("sampleTree.txt") {
      def createAccountRecord(ws: Array[String]): Option[AccountRecord] = AccountRecord.parse(ws(7), ws(5), ws(6))
    }
    checkTreeFromResource(TestDetailsSample, 113, 3, 64, 113, 47)
  }
  private def checkTreeFromResource(tester: AbstractTestDetails, size: Int, depth: Int, before: Int, iteratorSize: Int, mpttSize: Int) = {
    //    Spy.spying = true
    val aso = AccountRecordTest.readAccountData(tester)
    val checks = AccountRecordTest.checkAccountTree(size, depth, before, iteratorSize, mpttSize, aso)
    println(checks)
    checks should matchPattern { case Success((`size`,`depth`,Some(_),`before`,`iteratorSize`,`mpttSize`)) => }
  }

}

object AccountRecordTest {
  type NodeType = Value[AccountRecord]

  object NodeType {

    trait NodeTypeParent extends HasParent[Value[AccountRecord]] {
      def createParent(t: Value[AccountRecord]): Option[Node[Value[AccountRecord]]] = None

      def getParentKey(v: Value[AccountRecord]): Option[String] = Some(v.value.parent)
    }
    implicit object HasParentNodeType extends NodeTypeParent
  }

  def readAccountData(tester: AbstractTestDetails) = {
    val uo = Option(getClass.getResource(tester.resourceName))
    val so = uo map (_.openStream)
    val wsso = for (s <- so) yield for (l <- Source.fromInputStream(s).getLines) yield for (w <- l.split("""\|""")) yield w
    val aoso = for (wss <- wsso) yield for (ws <- wss) yield tester.createAccountRecord(ws)

    // CONSIDER Now, we flatten the options, resulting in None if there were any problems at all. May want to change the behavior of this later
    (for (aos <- aoso) yield FP.sequence(aos.toSeq)).flatten
  }

  def checkAccountTree(size: Int, depth: Int, before: Int, iteratorSize: Int, mpttSize: Int, aso: Option[Seq[AccountRecord]]) = {
    aso match {
      case Some(as) =>
        import AccountRecord._
        implicit object GeneralKVTreeBuilderNodeType extends GeneralKVTreeBuilder[AccountRecord]
        implicit object ValueBuilderNodeType extends ValueBuilder[AccountRecord] {
          // TODO fix this totally arbitrary value for date
          def buildValue(k: String): Value[AccountRecord] = Value(AccountRecord(k, AccountDate(1900, 1, 1), "root"))
        }
        implicit object NodeTypeParent extends HasParent[NodeType] {
          def getParentKey(t: NodeType): Option[String] = Some(t.value.parent)

          def createParent(t: NodeType): Option[Tree[NodeType]] = {
            val treeBuilder = implicitly[TreeBuilder[NodeType]]
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
            //            println(tree)
            println(tree.render())
            val ns = tree.nodeIterator(true)
            //            println(ns.toList)
            val lt: Int => Boolean = _ < 0
            val eq: Int => Boolean = _ == 0

            def compareWithDate(f: Int => Boolean)(d: AccountDate)(n: Node[NodeType]): Boolean = n.get match {
              case Some(Value(AccountRecord("root", _, _))) => false
              case Some(Value(v)) => f(v.date.compare(d))
              case _ => false
            }

            val onDate = compareWithDate(eq) _
            val beforeDate = compareWithDate(lt) _

            // TODO recreate this test
            val indexedTree = KVTree.createIndexedTree(tree)
            val mptt = MPTT(indexedTree)
            println(mptt)
            Success((tree.size,tree.depth,tree.find(onDate(AccountDate(2014, 9, 30))),tree.filter(beforeDate(AccountDate(2014, 9, 30))).size,indexedTree.nodeIterator(true).size,mptt.index.size))
          case f @ Failure(x) => f
        }



      case None => Failure(TreeException("unable to yield a complete hierarchy"))
    }
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
    * @tparam V the underlying type of the Values
    * @return the newly created tree
    */
  def populateParentChildTree[V](values: Seq[Value[V]])(implicit ev1: HasParent[Value[V]], ev2: NodeParent[Value[V]], treeBuilder: TreeBuilder[Value[V]], valueBuilder: ValueBuilder[V]): Try[Tree[Value[V]]] =
  {
    val root = valueBuilder.buildValue("root")
    val ty = Try(implicitly[TreeBuilder[Value[V]]].buildTree(Some(root), Seq()).asInstanceOf[KVTree[V]])
      @tailrec
      def inner(result: Try[Tree[Value[V]]], values: List[Value[V]]): Try[Tree[Value[V]]] = values match {
        case Nil => result
        case y :: z => inner(for (t <- result; u = t :+ y) yield u, z)
      }
      inner(ty, values.toList)
  }
}