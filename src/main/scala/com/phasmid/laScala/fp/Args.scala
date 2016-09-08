package com.phasmid.laScala.fp

import scala.util._

/**
  * Created by scalaprof on 9/7/16.
  */
case class Args(args: List[Any]) extends (() => Try[(Any,Args)]) {
  def apply(): Try[(Any, Args)] = args match {
    case Nil => Failure(new Exception("Args is empty: this typically means you didn't provide sufficient arguments"))
    case h :: t => Success(h, Args(t))
  }
  def get[T](clazz: Class[T]): Try[(T, Args)] = {
    apply match {
      case Success((r,a)) =>
        if (clazz.isInstance(r) || clazz.isAssignableFrom(r.getClass))
          Success(r.asInstanceOf[T], a)
        else throw new Exception(s"args head is not of type: $clazz but is of type ${r.getClass}")
      case f @ Failure(t) => f.asInstanceOf[Try[(T, Args)]]
    }
  }
  def isEmpty: Boolean = args.isEmpty
}
