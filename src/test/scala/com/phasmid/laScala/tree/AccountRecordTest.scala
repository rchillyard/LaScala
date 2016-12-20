package com.phasmid.laScala.tree

import com.phasmid.laScala.Kleenean
import com.phasmid.laScala.fp._
import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.io.Source
import scala.util._

case class AccountRecord(account: String, date: AccountDate, parent: String) {
  def key: String = s"$account/$date"
}

case class AccountDate(year: Int, month: Int, day: Int) extends Ordering[AccountDate] {
  def compare(other: AccountDate): Int = compare(this,other)
  def compare(x: AccountDate, y: AccountDate): Int =
    Kleenean(Kleenean(x.year.compareTo(y.year))() orElse Kleenean(x.month.compareTo(y.month))() orElse Kleenean(x.day.compareTo(y.day))()).toInt
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
  type NodeType = AccountRecord // XXX: can inline this

    implicit object AccountRecordKeyOps extends KeyOps[String,NodeType] {
      //  type K
      def getKeyFromValue(v: NodeType): String = v.key
      def getParentKey(v: NodeType): Option[String] = Some(v.parent)
    }
  implicit object NodeTypeParent extends HasParent[String,NodeType] {
    def getParent(tree: Tree[NodeType], t: NodeType): Option[Node[NodeType]] =
      for (k <- getParentKey(t); kVTree = tree.asInstanceOf[KVTree[String,AccountRecord]]; n <- kVTree.findByKey(k)) yield n

    def createValueFromKey(k: String): NodeType = null.asInstanceOf[NodeType] // FIXME

    def getParentKey(t: NodeType): Option[String] = Some(t.parent)

//    def createParent(t: NodeType): Option[Tree[NodeType]] = {
//      val treeBuilder = implicitly[TreeBuilder[NodeType]]
//      val vo = for (k <- getParentKey(t)) yield implicitly[ValueBuilder[AccountRecord]].buildValue(k)
//      for (_ <- vo) yield treeBuilder.buildTree(vo, Seq())
//    }
  }



  //  abstract class HasKeyAccountRecord extends HasKey[AccountRecord] {
//    def createValueFromKey(k: K): AccountRecord = null.asInstanceOf[AccountRecord] // FIXME
//
//    type K = String
//    def getKey(x: AccountRecord): K = s"${x.account}/${x.date}"
//  }
//  implicit object HasKeyAccountRecord extends HasKeyAccountRecord

  implicit object OrderingAccountRecord extends Ordering[AccountRecord] {
    def compare(x: AccountRecord, y: AccountRecord): Int = x.account.compareTo(y.account) match {
      case 0 => x.date.compare(y.date)
      case x @ _ => x
    }
  }
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
    checkTreeFromResource(TestDetailsMiniSample, 13, 3, 6, 13, 5)
  }
  // XXX we ignore this because I have not committed the sampleTree.txt file to the repository (and, in any case, this is more of a functional test)
  it should "work for sampleTree.txt" in {
    case object TestDetailsSample extends AbstractTestDetails("sampleTree.txt") {
      def createAccountRecord(ws: Array[String]): Option[AccountRecord] = AccountRecord.parse(ws(7), ws(5), ws(6))
    }
    checkTreeFromResource(TestDetailsSample, 201, 3, 114, 201, 24)
  }

  private def checkTreeFromResource(tester: AbstractTestDetails, size: Int, depth: Int, before: Int, iteratorSize: Int, mpttSize: Int) =
    {
      implicit val logger = Spy.getLogger(getClass)
      val aso = AccountRecordTest.readAccountData(tester)
      val checks = Spy.noSpy(AccountRecordTest.checkAccountTree(size, depth, before, iteratorSize, mpttSize, aso))
      checks should matchPattern { case Success((`size`,`depth`,`before`,`iteratorSize`,`mpttSize`,Some(_),_,_)) => }

      checks match {
        case Success((_,_,_,_,_,_,tree,mptt)) =>
          println(tree.size)
          println(tree)
          println(tree.render())
          println(mptt)
          val nodes: Seq[Node[AccountRecord]] = Spy.noSpy(tree.nodeIterator().toList)
          val (leafNodes,branchNodes) = nodes partition (_.isLeaf)
          val trues = for (n <- branchNodes; v <- n.get) yield Spy.noSpy(tree.includesValue(v))
          val allTrue = Spy.spy("all true",trues.forall(_==true))
          allTrue shouldBe true

          val values = Spy.noSpy(tree.iterator().toList)
          val results = Spy.noSpy(for (x <- values; y <- values) yield (x, y, mptt.contains(x.key, y.key).contains(tree.includes(x, y))))
          Spy.log("checking for disagreement between tree and MPTT")
          Spy.log(s"${(results filter (_._3)).size} results out of ${results.size}")
          results find (!_._3) match {
            case Some(r) =>
              val index = results indexWhere (!_._3)
              if (index>=0) Spy.log(s"failed on $index-th element out of ${results.size}")
              val message = s"the following did not agree: ${r._1.key} and ${r._2.key}. tree: ${tree.includes(r._1, r._2)}; mptt: ${mptt.contains(r._1.key, r._2.key)}"
              Spy.log(message)
              val (x,y,b) = results(index)
              val subtree = tree.find(x)
              val node = tree.find(y)
              Spy.spy(s"this is the (first) failure case: $subtree includes $node",tree.includes(subtree,node))
            case _ =>
          }
//          Spy.noSpy(AccountRecordTest.doBenchmark(tree,mptt))
        case _ => fail(s"checks did not come back as expected: $checks")
      }
    }
}

object AccountRecordTest {

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
        import AccountRecord._
        import GeneralKVTree._
        implicit object GeneralKVTreeBuilderNodeType extends GeneralKVTreeBuilder[String,AccountRecord]
//        implicit object ValueBuilderNodeType extends ValueBuilder[AccountRecord] {
//          // TODO fix this totally arbitrary value for date
//          def buildValue(k: String): Value[AccountRecord] = Value(AccountRecord(k, AccountDate(1900, 1, 1), "root"))
//        }

//        implicit object NodeTypeNodeParent extends NodeParent[NodeType] {
//          // XXX see comment on GeneralNodeParent
//          def isParent(parent: Node[NodeType], child: Node[NodeType]): Boolean = parent match {
//            case Branch(_,ns) => ns.contains(child)
//            case _ => false
//          }
//        }

        ParentChildTree.populateParentChildTree(as) match {
          case Success(tree) =>
            //            val ns = tree.nodeIterator(true)
            val lt: Int => Boolean = _ < 0
            val eq: Int => Boolean = _ == 0

            def compareWithDate(f: Int => Boolean)(d: AccountDate)(n: Node[NodeType]): Boolean = n.get match {
              case Some(AccountRecord("root", _, _)) => false
              case Some(v) => f(v.date.compare(d))
              case _ => false
            }

            val onDate = compareWithDate(eq) _
            val beforeDate = compareWithDate(lt) _

            import AccountRecord._
//            implicit object HasKeyValueAccountRecord extends HasKey[Value[AccountRecord]] {
//              def createValueFromKey(k: K): Value[AccountRecord] = null.asInstanceOf[Value[AccountRecord]] // FIXME
//
//              type K = String
//              def getKey(v: Value[AccountRecord]): String = v.key
//            }
            val indexedTree = Tree.createIndexedTree(tree)
            val mptt = MPTT.createValuedMPTT(indexedTree)
            Success((tree.size, tree.depth, tree.filter(beforeDate(AccountDate(2014, 9, 30))).size, indexedTree.nodeIterator().size, mptt.index.size, tree.find(onDate(AccountDate(2014, 9, 30))), tree, mptt))
          case Failure(x) => Failure(x)
        }

      case None => Failure(TreeException("unable to yield a complete hierarchy"))
    }
  }

  def doBenchmark(tree: Tree[AccountRecord], mptt: MPTT[AccountRecord]): Unit = {
    import AccountRecord._
//    implicit object ValueOrdering extends Ordering[NodeType] {
//      def compare(x: NodeType, y: NodeType): Int = implicitly[Ordering[AccountRecord]].compare(x,y)
//    }
    implicit object NodeOrdering extends Ordering[Node[NodeType]] {
      def compare(x: Node[NodeType], y: Node[NodeType]): Int = FP.map2(x.get,y.get)(implicitly[Ordering[NodeType]].compare).get
    }
    val nodes = tree.iterator().toList
    val snodes = nodes.sorted
    val pairs = nodes zip snodes
    val keys = nodes map (_.key) zip (snodes map (_.key))
    import Benchmark._
    val fIncludes: (NodeType,NodeType)=>(String,String,Boolean) = {(x,y) => (x.toString,y.toString,tree.includes(x,y))}
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
  def populateParentChildTree[V](values: Seq[V])(implicit treeBuilder: TreeBuilder[V], ko: KeyOps[String,V], hp: HasParent[String,V]): Try[Tree[V]] =
  {
//    val root = valueBuilder.buildValue("root")
    val ty = Try(implicitly[TreeBuilder[V]].buildTree(Some(hp.createValueFromKey("root")), Seq()).asInstanceOf[KVTree[String,V]])
      @tailrec
      def inner(result: Try[Tree[V]], values: List[V]): Try[Tree[V]] = values match {
        case Nil => result
        case y :: z => inner(for (t <- result; u = t :+ y) yield u, z)
      }
      inner(ty, values.toList)
  }
}