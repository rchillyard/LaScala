/*
 * LaScala
 * Copyright (c) 2016, 2017. Phasmid Software
 */

package com.phasmid.laScala

import com.phasmid.laScala.values.CaseClasses

import scala.collection.immutable.ListMap
import scala.language.implicitConversions
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}

/**
  * This trait defines an aspect of an object as being able to be rendered as a String.
  * The purposes of this alternative form of conversion to a String (in addition to the toString defined on Any),
  * are:
  * (1) to create a String explicitly for human-readability;
  * (2) to allow for (relative) indentation of complex data structures such as trees.
  *
  * Created by scalaprof on 1/7/17.
  */
trait Renderable {

  /**
    * Method to render this object in a human-legible manner.
    *
    * NOTE: implementations of render may invoke toString
    *
    * NOTE: toString may NOT call render
    *
    * @param indent the number of "tabs" before output should start (when writing on a new line).
    * @param tab    an implicit function to translate the tab number (i.e. indent) to a String of white space.
    *               Typically (and by default) this will be uniform. But you're free to set up a series of tabs
    *               like on an old typewriter where the spacing is non-uniform.
    * @return a String that, if it has embedded newlines, will follow each newline with (possibly empty) white space,
    *         which is then followed by some human-legible rendition of *this*.
    */
  def render(indent: Int = 0)(implicit tab: Int => Prefix): String

  /**
    * Method to insert an indented (tabbed) newline
    *
    * @return a string which is a suitable replacement for newline character
    */
  def nl(indent: Int)(implicit tab: Int => Prefix): String = "\n" + tab(indent)
}

/**
  * Case class which defines a Renderable Traversable object.
  *
  * @param xs       the traversable to be rendered
  * @param bookends the prefix, separator, and suffix
  * @param linear   true if we want the results all on one line; default is false
  */
case class RenderableTraversable(xs: Traversable[_], bookends: String = "(,)", linear: Boolean = false, max: Option[Int] = None) extends Renderable {
  def render(indent: Int)(implicit tab: (Int) => Prefix): String = {
    val (p, q, r) =
      if (linear || xs.size <= 1)
        ("" + bookends.head, "" + bookends.tail.head, "" + bookends.last)
      else
        (bookends.head + nl(indent + 1), bookends.tail.head + nl(indent + 1), nl(indent) + bookends.last)
    Renderable.elemsToString(xs, indent, p, q, r, max)
  }
}

/**
  * Case class which defines a Renderable Product object.
  *
  * @param p the product/tuple to be rendered
  */
case class RenderableProduct(p: Product) extends Renderable {
  def render(indent: Int)(implicit tab: (Int) => Prefix): String = Renderable.elemsToString(p.productIterator.toSeq, indent, s"${p.productPrefix}(", ",", ")")
}

/**
  * Case class which defines a Renderable Product object.
  *
  * @param t the case class to be rendered
  */
case class RenderableCaseClass[T: TypeTag](t: T) extends Renderable {

  def render(indent: Int = 0)(implicit tab: (Int) => Prefix): String = {
    val p = t.asInstanceOf[Product]
    val sb = new StringBuilder(s"${p.productPrefix}(")
    // TODO figure out why it doesn't work to invoke RenderableCaseClass.getParameters
    val parameters = CaseClasses.caseClassParamsOf zip t.asInstanceOf[Product].productIterator.toSeq
    // TODO need to ensure we don't show any parameters not in the first parameter set
    for (((k, _), v) <- parameters) sb.append(nl(indent + 1) + s"$k:" + Renderable.renderElem(v, indent + 1))
    sb.append(nl(indent + 1) + ")")
    sb.toString()
  }
}

object RenderableCaseClass {
  // TODO make this private -- it is used only for unit testing currently so it must match the corresponding line in render above.
  def getParameters[T: TypeTag](t: T): ListMap[(String, Option[universe.Type]), Any] = CaseClasses.caseClassParamsOf zip t.asInstanceOf[Product].productIterator.toSeq
}
/**
  * Case class which defines a RenderableOption object.
  *
  * @param xo the optional value
  */
case class RenderableOption(xo: Option[_]) extends Renderable {
  def render(indent: Int)(implicit tab: (Int) => Prefix): String = xo match {
    case Some(x) => s"Some(" + Renderable.renderElem(x, indent) + ")"
    case _ => "None"
  }
}

/**
  * Case class which defines a Renderable Try object.
  *
  * @param xy the try value
  */
case class RenderableTry(xy: Try[_]) extends Renderable {

  def render(indent: Int)(implicit tab: (Int) => Prefix): String = xy match {
    case Success(x) => s"Success(" + Renderable.renderElem(x, indent) + ")"
    case Failure(t) => s"Failure(${t.getLocalizedMessage})"
  }
}

/**
  * Case class which defines a Renderable Either object.
  *
  * @param e the either value
  */
case class RenderableEither(e: Either[_, _]) extends Renderable {

  def render(indent: Int)(implicit tab: (Int) => Prefix): String = e match {
    case Left(x) => s"Left(" + Renderable.renderElem(x, indent) + ")"
    case Right(x) => s"Right(" + Renderable.renderElem(x, indent) + ")"
  }
}

object Renderable {

  /**
    * This is the default maximum number of elements that will be rendered by a Traversable (see renderableTraversable).
    */
  val MAX_ELEMENTS = 10

  /**
    * This default implementation of RenderableTraversable limits the number of elements to MAX_ELEMENTS.
    *
    * @param xs the Traversable to render
    * @return a Renderable
    */
  implicit def renderableTraversable(xs: Traversable[_]): Renderable = RenderableTraversable(xs, max = Some(MAX_ELEMENTS))

  implicit def renderableOption(xo: Option[_]): Renderable = RenderableOption(xo)

  implicit def renderableTry(xy: Try[_]): Renderable = RenderableTry(xy)

  implicit def renderableEither(e: Either[_, _]): Renderable = RenderableEither(e)

  implicit def renderableProduct(p: Product): Renderable = RenderableProduct(p)

  def renderElem(elem: Any, indent: Int)(implicit tab: (Int) => Prefix): String = elem match {
    case xo: Option[_] => renderableOption(xo).render(indent + 1)
    case xs: Traversable[_] => renderableTraversable(xs).render(indent + 1)
    case xy: Try[_] => renderableTry(xy).render(indent + 1)
    case e: Either[_, _] => renderableEither(e).render(indent + 1)
    case s: String => s""""$s""""
    case r: Renderable => r.render(indent + 1)
    case x => x.toString
  }

  def elemsToString(xs: Traversable[_], indent: Int, start: String, sep: String, end: String, max: Option[Int] = None)(implicit tab: (Int) => Prefix): String = {
    var first = true
    val b = new StringBuilder()
    b append start
    val z = max match {
      case Some(n) => xs take n
      case None => xs
    }
    for (x <- z) {
      if (first) {
        b append renderElem(x, indent)
        first = false
      }
      else {
        b append sep
        b append renderElem(x, indent)
      }
    }
    max match {
      case Some(n) => if (n < xs.size) b append s"$sep...[${xs.size - n} more elements]"
      case None =>
    }
    b append end
    b.toString()
  }

}

case class Prefix(s: String) {
  override def toString: String = s
}

object Prefix {

  /**
    * The default tab method which translates indent uniformly into that number of double-spaces.
    *
    * @param indent the number of tabs before output should start on a new line.
    * @return a String of white space.
    */
  implicit def tab(indent: Int): Prefix = Prefix(" " * indent * 2)
}
