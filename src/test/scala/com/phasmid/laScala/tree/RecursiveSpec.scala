package com.phasmid.laScala.tree

import com.phasmid.laScala.Kleenean
import com.phasmid.laScala.fp.{FP, HasKey}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source
import scala.util._

case class AccountRecord(date: AccountDate, parent: String, account: String)

case class AccountDate(year: Int, month: Int, day: Int) extends Ordering[AccountDate] {
  def compare(other: AccountDate): Int = compare(this,other)
  def compare(x: AccountDate, y: AccountDate): Int =
    Kleenean(Kleenean(x.year.compareTo(y.year))() orElse Kleenean(x.month.compareTo(y.month))() orElse Kleenean(x.day.compareTo(y.day))()).toInt
}

object AccountRecord {
  def parse(date: String, parent: String, account: String): Option[AccountRecord] = {
    val nodeR = """([A-Z\d-]{20})""".r
    val accountR = """([A-Z\d-]{4,20})""".r
    val dateR = """(\d{4})-(\d{2})-(\d{2})""".r
    val p = parent match {
      case nodeR(x) => Success(x)
      case _ => Failure(TreeException(s"parent node string didn't match: $parent"))
    }
    val a = account match {
      case accountR(x) => Success(x)
      case _ => Failure(TreeException(s"account string didn't match: $account"))
    }
    val d = date match {
      case dateR(y,m,n) => AccountDate.parse(y,m,n)
    }
    FP.toOption(FP.map3(d,p,a)(apply))
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

  object NodeType {
    abstract class HasKeyNodeValue extends HasKey[NodeType] {
      def getKey(v: NodeType): String = v.key

      type K = String
    }
    implicit object HasKeyNodeValue extends HasKeyNodeValue

  }

  behavior of "Recursive account lookup"
  it should "work" in {

    val uo = Option(getClass.getResource("sampleTree.txt"))
    uo should matchPattern { case Some(_) => }
    val so = uo map ( _.openStream )
    val wsso = for (s <- so) yield for (l <- Source.fromInputStream(s).getLines) yield for (w <- l.split("""\|""")) yield w
    val aoso = for (wss <- wsso) yield for(ws <- wss) yield AccountRecord.parse(ws(5), ws(6), ws(7))

    // CONSIDER Now, we flatten the options, resulting in None if there were any problems at all. May want to change the behavior of this later
    val aso = (for (aos <- aoso) yield FP.sequence(aos.toSeq)).flatten
    aso match {
      case Some(as) =>
        import AccountRecord._
        import GeneralTree._
        implicit object GeneralTreeBuilderValue extends GeneralTreeBuilder[NodeType]
        implicit object GeneralLeafBuilderValue extends GeneralLeafBuilder[NodeType]

        val tree = KVTree.populateGeneralTree (as map Value[String, AccountRecord])
        tree.size shouldBe 100
//        tree.depth shouldBe 2
        val ns = tree.nodeIterator(true)
        println(ns.toList)
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
        import NodeType._
        val mptt = MPTT (indexedTree)
        mptt.index.size shouldBe 34
        println (mptt)

      case None => System.err.println("unable to yield a complete hierarchy")
    }
  }
}

