package com.phasmid.laScala.fp

import scala.util._

/**
  * Created by scalaprof on 9/7/16.
  */
case class Args(args: List[Any]) extends (() => Try[(Any, Args)]) {
  def apply(): Try[(Any, Args)] = args match {
    case Nil => Failure(new Exception("Args is empty: this typically means you didn't provide sufficient arguments"))
    case h :: t => Success(h, Args(t))
  }

  def get[T](clazz: Class[T]): Try[(T, Args)] = {
    apply() match {
      case Success((r, a)) =>
        if (clazz.isInstance(r) || clazz.isAssignableFrom(r.getClass))
          Success(r.asInstanceOf[T], a)
        //        else if (r.isInstanceOf[String])
        //          Try (r.t)
        else throw new Exception(s"args head is not of type: $clazz but is of type ${r.getClass}")
      case f@Failure(_) => f.asInstanceOf[Try[(T, Args)]]
    }
  }

  def isEmpty: Boolean = args.isEmpty

  override def toString: String = args mkString ", "
}

object Args {

  def apply(seq: Any*): Args = apply(seq.toList)

//    val x = Args(args.toList)
//
//    def inner(as: Args, cs: List[Class[_]], result: List[Any]): List[Any] = {
//      if (as.isEmpty) result
//      else cs match {
//        case Nil => throw new Exception(s"insufficient class parameters specified")
//        case h :: t =>
//          as.get(h) match {
//            case Success((y, bs)) => inner(bs, t, result :+ y)
//            case Failure(z) => throw z
//          }
//      }
//    }
//
//    val result = inner(x, List(classOf[String], classOf[String], classOf[Int]), List())
//    //    val (a: String) :: (b: String) :: (c: Int) :: Nil = result
//    val a :: b :: c :: Nil = result
//    println(s"$a @ ${a.getClass}")
//    println(s"$b @ ${b.getClass}")
//    println(s"$c @ ${c.getClass}")
}