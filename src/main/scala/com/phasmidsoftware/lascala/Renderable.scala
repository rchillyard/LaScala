/*
 * LaScala
 * Copyright (c) 2016, 2017, 2026. Phasmid Software
 */

package com.phasmidsoftware.lascala

import scala.compiletime.{constValue, erasedValue, summonFrom, summonInline}
import scala.deriving.Mirror
import scala.util.{Failure, Success, Try}

/**
  * Type class to render an A in a human-legible manner (as an alternative to toString),
  * with nested structures indented according to the Layout.
  *
  * Instances for the common types are in the companion object; a case class, sealed trait or enum can write
  * <code>derives Renderable</code>.
  *
  * NOTE: implementations of render may invoke toString, but toString may NOT call render.
  *
  * @tparam A the type to be rendered
  */
trait Renderable[A] {

  /**
    * Method to render a.
    *
    * @param a      the value to be rendered
    * @param indent the number of tabs (see Layout) by which any lines after the first are indented
    * @return a String that, if it has embedded newlines, will follow each newline with (possibly empty) white space,
    *         which is then followed by some human-legible rendering of a.
    */
  def render(a: A, indent: Int)(using Layout): String

  /**
    * Extension method to render a (starting with no indentation).
    *
    * NOTE: this cannot take an indent parameter, because it would then erase to the same signature as render(a, indent).
    * For a starting indent, invoke render(a, indent) on the instance.
    */
  extension (a: A) def render(using Layout): String = this.render(a, 0)
}

/**
  * How a rendering is laid out.
  *
  * @param tab         the white space for a given indent (by default, two spaces per tab).
  *                    Typically this will be uniform, but you're free to set up a series of tabs
  *                    like on an old typewriter where the spacing is non-uniform.
  * @param maxElements the maximum number of elements of a collection to be rendered (by default, 10)
  * @param bookends    the prefix, separator and suffix of a collection (by default, "(,)")
  */
final case class Layout(tab: Int => String = Layout.uniform(2), maxElements: Option[Int] = Some(Layout.MAX_ELEMENTS), bookends: String = "(,)") {
  require(bookends.length == 3, s"bookends must have three characters: $bookends")

  /**
    * Method to insert an indented (tabbed) newline.
    *
    * @param indent the number of tabs
    * @return a newline followed by the white space for indent
    */
  def nl(indent: Int): String = "\n" + tab(indent)
}

object Layout {
  /**
    * This is the default maximum number of elements that will be rendered for a collection.
    */
  val MAX_ELEMENTS = 10

  def uniform(width: Int): Int => String = indent => " " * (indent * width)

  /**
    * The default Layout, which is found without an import.
    * To use a different one, declare (in an inner block, because a given applies to the whole of its block) a given Layout.
    */
  given default: Layout = Layout()
}

object Renderable {

  def apply[A](using r: Renderable[A]): Renderable[A] = r

  /**
    * Method to create a Renderable[A] that uses toString.
    * This is never used automatically: a type with no Renderable instance does not compile.
    *
    * @tparam A the underlying type
    * @return a Renderable[A]
    */
  def fromToString[A]: Renderable[A] = new Renderable[A] {
    def render(a: A, indent: Int)(using Layout): String = a.toString
  }

  given intRenderable: Renderable[Int] = fromToString

  given longRenderable: Renderable[Long] = fromToString

  given doubleRenderable: Renderable[Double] = fromToString

  given floatRenderable: Renderable[Float] = fromToString

  given booleanRenderable: Renderable[Boolean] = fromToString

  given charRenderable: Renderable[Char] = fromToString

  given bigIntRenderable: Renderable[BigInt] = fromToString

  given bigDecimalRenderable: Renderable[BigDecimal] = fromToString

  given unitRenderable: Renderable[Unit] = new Renderable[Unit] {
    def render(u: Unit, indent: Int)(using Layout): String = "()"
  }

  /**
    * Strings are quoted, so that they can be distinguished within a structure.
    */
  given stringRenderable: Renderable[String] = new Renderable[String] {
    def render(w: String, indent: Int)(using Layout): String = s""""$w""""
  }

  given optionRenderable[A](using r: Renderable[A]): Renderable[Option[A]] = new Renderable[Option[A]] {
    def render(ao: Option[A], indent: Int)(using Layout): String = ao match {
      case Some(a) => s"Some(${r.render(a, indent + 1)})"
      case None => "None"
    }
  }

  given tryRenderable[A](using r: Renderable[A]): Renderable[Try[A]] = new Renderable[Try[A]] {
    def render(ay: Try[A], indent: Int)(using Layout): String = ay match {
      case Success(a) => s"Success(${r.render(a, indent + 1)})"
      case Failure(t) => s"Failure(${t.getLocalizedMessage})"
    }
  }

  given eitherRenderable[L, R](using lr: Renderable[L], rr: Renderable[R]): Renderable[Either[L, R]] = new Renderable[Either[L, R]] {
    def render(e: Either[L, R], indent: Int)(using Layout): String = e match {
      case Left(l) => s"Left(${lr.render(l, indent + 1)})"
      case Right(r) => s"Right(${rr.render(r, indent + 1)})"
    }
  }

  /**
    * One instance for every collection (List, Vector, Seq, Set, LazyList, etc.).
    */
  given iterableRenderable[A, CC[X] <: Iterable[X]](using r: Renderable[A]): Renderable[CC[A]] = new Renderable[CC[A]] {
    def render(as: CC[A], indent: Int)(using layout: Layout): String = renderElements(as.toSeq.map(a => r.render(a, indent + 1)), indent)
  }

  /**
    * A Map is rendered as a collection of key->value entries.
    */
  given mapRenderable[K, V](using kr: Renderable[K], vr: Renderable[V]): Renderable[Map[K, V]] = new Renderable[Map[K, V]] {
    def render(m: Map[K, V], indent: Int)(using Layout): String =
      renderElements(m.toSeq.map((k, v) => kr.render(k, indent + 1) + "->" + vr.render(v, indent + 1)), indent)
  }

  /**
    * Tuples are rendered on one line, for example Tuple2("x","y").
    */
  // NOTE: the Mirror is summoned inside (rather than being a using parameter) so that T is known when it is looked for.
  inline given tupleRenderable[T <: Tuple]: Renderable[T] = summonFrom {
    case m: Mirror.ProductOf[T] => linearProductRenderable[T](constValue[m.MirroredLabel], elementRenderables[m.MirroredElemTypes])
  }

  /**
    * Method to lay out elements which have already been rendered (each at indent + 1).
    * If there are at least two elements, each starts on a new line; otherwise, they are all on one line.
    *
    * @param ws     the rendered elements
    * @param indent the indent of the collection
    * @return the elements with bookends and separators, and a note of any elements beyond layout.maxElements
    */
  def renderElements(ws: Seq[String], indent: Int)(using layout: Layout): String = {
    val Seq(open, sep, close) = layout.bookends.toSeq.map(_.toString)
    val (start, separator, end) = if (ws.size <= 1) (open, sep, close) else (open + layout.nl(indent + 1), sep + layout.nl(indent + 1), layout.nl(indent) + close)
    val shown = layout.maxElements.fold(ws)(ws.take)
    val more = if (shown.size < ws.size) s"$separator...[${ws.size - shown.size} more elements]" else ""
    shown.mkString(start, separator, more + end)
  }

  /**
    * Method to derive a Renderable for a case class (or case object), sealed trait or enum.
    * A case class is rendered with one line per field, for example:
    * <pre>
    * Player(
    *   name:"Robin",
    *   rating:1850.5
    * )
    * </pre>
    */
  inline def derived[T](using m: Mirror.Of[T]): Renderable[T] = inline m match {
    case s: Mirror.SumOf[T] => sumRenderable(s, caseRenderables[s.MirroredElemTypes])
    case p: Mirror.ProductOf[T] => productRenderable(constValue[p.MirroredLabel], labels[p.MirroredElemLabels], elementRenderables[p.MirroredElemTypes])
  }

  // The element instances are passed by name, and evaluated lazily, so that a recursive type can use its own instance while it is still being defined.
  private def productRenderable[T](name: String, fieldNames: List[String], rs: => List[Renderable[?]]): Renderable[T] = new Renderable[T] {
    lazy val elementRenderables: List[Renderable[?]] = rs

    def render(t: T, indent: Int)(using layout: Layout): String =
      if (fieldNames.isEmpty) name
      else {
        val values = renderValues(t.asInstanceOf[Product], elementRenderables, indent + 1)
        val fields = fieldNames.zip(values).map((k, v) => layout.nl(indent + 1) + s"$k:$v")
        fields.mkString(s"$name(", ",", layout.nl(indent) + ")")
      }
  }

  private def linearProductRenderable[T](name: String, rs: => List[Renderable[?]]): Renderable[T] = new Renderable[T] {
    lazy val elementRenderables: List[Renderable[?]] = rs

    def render(t: T, indent: Int)(using Layout): String =
      renderValues(t.asInstanceOf[Product], elementRenderables, indent + 1).mkString(s"$name(", ",", ")")
  }

  private def renderValues(p: Product, rs: List[Renderable[?]], indent: Int)(using Layout): List[String] =
    p.productIterator.toList.zip(rs).map((x, r) => r.asInstanceOf[Renderable[Any]].render(x, indent))

  private def sumRenderable[T](s: Mirror.SumOf[T], rs: => List[Renderable[?]]): Renderable[T] = new Renderable[T] {
    lazy val caseRenderables: List[Renderable[?]] = rs

    def render(t: T, indent: Int)(using Layout): String = caseRenderables(s.ordinal(t)).asInstanceOf[Renderable[T]].render(t, indent)
  }

  private inline def labels[Ls <: Tuple]: List[String] = inline erasedValue[Ls] match {
    case _: EmptyTuple => Nil
    case _: (l *: ls) => constValue[l].asInstanceOf[String] :: labels[ls]
  }

  private inline def elementRenderables[Ts <: Tuple]: List[Renderable[?]] = inline erasedValue[Ts] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[Renderable[t]] :: elementRenderables[ts]
  }

  private inline def caseRenderables[Ts <: Tuple]: List[Renderable[?]] = inline erasedValue[Ts] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts) => caseRenderable[t] :: caseRenderables[ts]
  }

  // Prefer a case's own given instance; otherwise derive one for it.
  private inline def caseRenderable[A]: Renderable[A] = summonFrom {
    case r: Renderable[A] => r
    case _ => derived[A](using summonInline[Mirror.Of[A]])
  }
}
