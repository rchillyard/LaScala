package com.phasmid.laScala.tree

import com.phasmid.laScala.Kleenean
import com.phasmid.laScala.fp._
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.io.Source
import scala.util._

case class AccountRecord(account: String, date: AccountDate, parent: String) {
  /**
    * Get the key which is used for accessing nodes and also for comparing nodes for the purpose of building an ordered tree
    * @return the key based on this AccountRecord
    */
  def key: String = s"$account/$date/$parent"
}

case class AccountDate(year: Int, month: Int, day: Int) {
  override def toString = f"$year%4d/$month%02d/$day%02d"
}

object AccountDate {
  def parse(y: String, m: String, d: String): Try[AccountDate] = FP.map3(Try(y.toInt), Try(m.toInt), Try(d.toInt))(apply)
  implicit object AccountDateOrdering extends Ordering[AccountDate] {
    def compare(x: AccountDate, y: AccountDate): Int =
      Kleenean(Kleenean(x.year.compareTo(y.year))() orElse Kleenean(x.month.compareTo(y.month))() orElse Kleenean(x.day.compareTo(y.day))()).toInt
  }
}
object AccountRecord {
  def apply(account: String): AccountRecord = apply(account, AccountDate(1900, 1, 1), "root")
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

  implicit object OrderingAccountRecord extends Ordering[AccountRecord] {
    def compare(x: AccountRecord, y: AccountRecord): Int = x.key.compareTo(y.key)
  }
}


trait BuildAccountRecord {
  def createAccountRecord(ws: Array[String]): Option[AccountRecord]
}
abstract class AbstractTestDetails(val resourceName: String) extends BuildAccountRecord

/**
  * Created by scalaprof on 10/19/16.
  */
class AccountRecordTest extends FlatSpec with Matchers {

  behavior of "AccountDate"

  it should "form correct string" in {
    AccountDate(2016,12,31).toString shouldBe "2016/12/31"
    AccountDate(2016,4,1).toString shouldBe "2016/04/01"
  }

  behavior of "AccountRecord"

  it should "form correct string" in {
    AccountRecord("Account#1",AccountDate(2016,12,31),"Account#0").toString shouldBe "AccountRecord(Account#1,2016/12/31,Account#0)"
  }

  it should "form correct key" in {
    AccountRecord("Account#1",AccountDate(2016,12,31),"Account#0").key.toString shouldBe "Account#1/2016/12/31/Account#0"
  }

  it should "compare correctly" in {
    val ac1q3 = AccountRecord("Account#1",AccountDate(2016,9,31),"Account#0")
    val ac1q4 = AccountRecord("Account#1",AccountDate(2016,12,31),"Account#0")
    implicit val ord = AccountRecord.OrderingAccountRecord
    ord.compare(ac1q3,ac1q4) shouldBe -1
    ac1q3.key.compareTo(ac1q4.key) shouldBe -1
  }

  behavior of "Recursive account lookup"

  // NOTE: there is an identical unit test in the it branch (integration tests). This one is too slow to have in unit tests.
  ignore should "work for miniSampleTree.txt" in {
    case object TestDetailsMiniSample extends AbstractTestDetails("miniSampleTree.txt") {
      def createAccountRecord(ws: Array[String]): Option[AccountRecord] = AccountRecord.parse(ws(2), ws(0), ws(1))
    }
    checkTreeFromResource(TestDetailsMiniSample, 9, 3, 5, 9, 9)
  }
  // XXX we ignore this because I have not committed the sampleTree.txt file to the repository (and, in any case, this is more of a functional test)
  ignore should "work for sampleTree.txt" in {
    case object TestDetailsSample extends AbstractTestDetails("sampleTree.txt") {
      def createAccountRecord(ws: Array[String]): Option[AccountRecord] = AccountRecord.parse(ws(7), ws(5), ws(6))
    }
    checkTreeFromResource(TestDetailsSample, 113, 3, 64, 113, 113)
  }

  private def checkTreeFromResource(tester: AbstractTestDetails, size: Int, depth: Int, before: Int, iteratorSize: Int, mpttSize: Int) =
    {
      implicit val logger = Spy.getLogger(getClass)
      val aso = AccountRecordTest.readAccountData(tester)
      val checks = Spy.noSpy(AccountRecordTest.checkAccountTree(size, depth, before, iteratorSize, mpttSize, aso))
      checks should matchPattern { case Success((`size`,`depth`,`before`,`iteratorSize`,`mpttSize`,Some(_),_,_)) => }
      checks match {
        case Success((_,_,_,_,_,_,tree,mptt)) =>
          val nodes: Seq[Node[AccountRecord]] = Spy.noSpy(tree.nodeIterator().toList)
          val (_,branchNodes) = nodes partition (_.isLeaf)
          val trues = for (n <- branchNodes; v <- n.get) yield Spy.noSpy(tree.includesValue(v))
          val allTrue = trues.forall(_ == true)
          allTrue shouldBe true
          val values = Spy.noSpy(tree.iterator().toList)
          val results = Spy.noSpy(for (x <- values; y <- values) yield (x, y, FP.contains(mptt.contains(x.key, y.key),tree.includes(x, y))))
          Spy.log(s"checking for disagreement between tree and MPTT:")
          // XXX: The following part of the test only works with Scala 2.11
//          val badResults = results filter (!_._3)
//          Spy.log(s"${badResults.size} incompatible results out of ${results.size}")
//          def show(x: AccountRecord, y: AccountRecord, z: Boolean) = {if (!z) println(s"$x, $y, ${mptt.contains(x.key, y.key)},${tree.includes(x, y)}")}
//          Spy.noSpy((badResults take 10) foreach (show _).tupled)
//          results find (!_._3) match {
//            case Some(r) =>
//              val index = results indexWhere (!_._3)
//              if (index>=0) Spy.log(s"failed on $index-th element out of ${results.size}")
//              val message = s"the following did not agree: ${r._1.key} and ${r._2.key}. tree: ${tree.includes(r._1, r._2)}; mptt: ${mptt.contains(r._1.key, r._2.key)}"
//              logger.info(message)
//              val (x,y, _) = results(index)
//              val subtree = tree.find(x)
//              val node = tree.find(y)
//              logger.info(s"this is the (first) failure case: $subtree includes $node " + tree.includes(subtree, node))
////              val ok = values==values.sorted
////              Spy.log(s"ok: $ok")
////              (values.sorted take 25) foreach println
////              (values take 25) foreach println
////              assert(index == -1,"there are inconsistencies between tree and MPTT")
//            case _ =>
//          }
          Spy.noSpy(AccountRecordTest.doBenchmark(tree,mptt))
        case _ => fail(s"checks did not come back as expected: $checks")
      }
    }
}

object AccountRecordTest {

  trait AccountRecordValueOps extends StringValueOps[AccountRecord] {
    def getParentKey(v: AccountRecord): Option[String] = Some(v.parent)

    override def getKeyAsParent(v: AccountRecord): String = v.account

    def createValueFromKey(k: String): Option[AccountRecord] = Some(AccountRecord(k,AccountDate(1900,1,1),"root"))
  }

  implicit object AccountRecordValueOps$ extends AccountRecordValueOps {
    override def getKeyFromValue(v: AccountRecord): String = v.key
  }

  def readAccountData(tester: AbstractTestDetails): Option[Seq[AccountRecord]] = {
    val uo = Option(getClass.getResource(tester.resourceName))
    val so = uo map (_.openStream)
    val wsso = for (s <- so) yield for (l <- Source.fromInputStream(s).getLines) yield for (w <- l.split("""\|""")) yield w
    val aoso = for (wss <- wsso) yield for (ws <- wss) yield tester.createAccountRecord(ws)

    // CONSIDER Now, we flatten the options, resulting in None if there were any problems at all. May want to change the behavior of this later
    (for (aos <- aoso) yield FP.sequence(aos.toSeq)).flatten
  }

  def checkAccountTree(size: Int, depth: Int, before: Int, iteratorSize: Int, mpttSize: Int, aso: Option[Seq[AccountRecord]]): Try[(Int,Int,Int,Int,Int,Option[Node[AccountRecord]],Tree[AccountRecord],MPTT[AccountRecord])] = {
    aso match {
      case Some(as) =>
        createAccountRecordTree(as) match {
          case Success(tree) =>
            //            val ns = tree.nodeIterator(true)
            val lt: Int => Boolean = _ < 0
            val eq: Int => Boolean = _ == 0

            def compareWithDate(f: Int => Boolean)(d: AccountDate)(n: Node[AccountRecord]): Boolean = n.get match {
              case Some(AccountRecord("root", _, _)) => false
              case Some(v) => f(implicitly[Ordering[AccountDate]].compare(v.date,d))
              case _ => false
            }

            val onDate = compareWithDate(eq) _
            val beforeDate = compareWithDate(lt) _
            val indexedTree = Tree.createIndexedTree(tree)
            val mptt = MPTT(indexedTree)
            Success((tree.size, tree.depth, tree.filter(beforeDate(AccountDate(2014, 9, 30))).size, indexedTree.nodeIterator().size, mptt.index.size, tree.find(onDate(AccountDate(2014, 9, 30))), tree, mptt))
          case Failure(x) => Failure(x)
        }

      case None => Failure(TreeException("unable to yield a complete hierarchy"))
    }
  }

  private def createAccountRecordTree(as: Seq[AccountRecord]) = {
    implicit object GeneralKVTreeBuilderAccountRecord extends GeneralKVTreeBuilder[String,AccountRecord]

    ParentChildTree.populateParentChildTree(as)
  }

  def doBenchmark(tree: Tree[AccountRecord], mptt: MPTT[AccountRecord]): Unit = {
    val nodes = tree.iterator().toList
    val snodes = nodes.sorted
    val pairs = nodes zip snodes
    val keys = nodes map (_.key) zip (snodes map (_.key))
    import Benchmark._
    val fIncludes: (AccountRecord,AccountRecord)=>(String,String,Boolean) = {(x,y) => (x.toString,y.toString,tree.includes(x,y))}
    10000.times(pairs map fIncludes.tupled)
    val ns1 = 10000.times(pairs map fIncludes.tupled)
    println(s"Benchmark time for tree.includes (nanosecs) = $ns1")
    //    val r1 = pairs map fIncludes.tupled
//    for (r <- r1) println(s"${r._1},${r._2},${r._3}")

    val fContains: (String,String)=>(String,String,Boolean) = {(x,y) => (x,y,mptt.contains(x,y).get)}
    10000.times(keys map fContains.tupled)
    val ns2 = 10000.times(keys map fContains.tupled)
    println(s"Benchmark time for mptt.contains (nanosecs) = $ns2")
    //    val r2 = keys map fContains.tupled
//    for (r <- r2) println(s"${r._1},${r._2},${r._3}")
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
  def populateParentChildTree[V](values: Seq[V])(implicit treeBuilder: TreeBuilder[V], vo: ValueOps[String,V]): Try[Tree[V]] =
  {
    val ty = Try(TreeBuilder[V].buildTree(vo.createValueFromKey("root"), Seq()).asInstanceOf[KVTree[String,V]])
      @tailrec
      def inner(result: Try[Tree[V]], values: List[V]): Try[Tree[V]] = values match {
        case Nil => result
        case y :: z => inner(for (t <- result; u = t :+ y) yield u, z)
      }
      inner(ty, values.toList)
  }
}