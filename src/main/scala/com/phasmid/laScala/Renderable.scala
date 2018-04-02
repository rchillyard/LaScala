package com.phasmid.laScala

import com.phasmid.laScala.values.Scalar

import scala.util.Try

trait Renderable[A] {
  /**
    * Method to render this object in a human-legible manner.
    *
    * NOTE: implementations of render may invoke toString
    *
    * NOTE: toString may NOT call render
    *
    * @param indent the number of "tabs" before output should start (when writing on a new line).
    * @return a String that, if it has embedded newlines, will follow each newline with (possibly empty) white space,
    *         which is then followed by some human-legible rendering of *this*.
    */
  def render(a: A)(indent: Int = 0): String

  /**
    * Method to translate the tab number (i.e. indent) to a String of white space.
    * Typically (and by default) this will be uniform. But you're free to set up a series of tabs
    * like on an old typewriter where the spacing is non-uniform.
    *
    * @return a String of white spade.
    */
  def tab(x: Int): String = " " * x * 2

}

object RenderableInstances {
  implicit val stringRenderable: Renderable[String] = new Renderable[String] {
    def render(w: String)(indent: Int): String = w replaceAll("""\n""", tab(indent).toString)
  }

  implicit val intRenderable: Renderable[Int] = new Renderable[Int] {
    def render(x: Int)(indent: Int): String = x.toString
  }

  implicit val doubleRenderable: Renderable[Double] = new Renderable[Double] {
    def render(x: Double)(indent: Int): String = x.toString
  }

  implicit val scalarRenderable: Renderable[Scalar] = new Renderable[Scalar] {
    def render(s: Scalar)(indent: Int): String = s.render(indent)
  }
}

object Renderable {
  def render[A](a: A)(indent: Int = 0)(implicit p: Renderable[A]): String = p.render(a)(indent)

  implicit def renderOption[A](implicit r: Renderable[A]): Renderable[Option[A]] = new Renderable[Option[A]] {
    def render(ao: Option[A])(indent: Int): String = ao map { a => s"Some(${r.render(a)(indent)})" } getOrElse "None"
  }

  implicit def renderTry[A](implicit r: Renderable[A]): Renderable[Try[A]] = new Renderable[Try[A]] {
    def render(ay: Try[A])(indent: Int): String = (ay map { a: A => s"Success(${r.render(a)(indent)})" }).recover { case t => s"Failure(${t.getLocalizedMessage})" }.get
  }

  implicit def renderEitherLeft[A,B](implicit r: Renderable[A]): Renderable[Either[A,B]] = new Renderable[Either[A,B]] {
    def render(ae: Either[A,B])(indent: Int): String = (ae.left map { a: A => s"Left(${r.render(a)(indent)})" }).fold(identity,{ b: B => s"Right: ${b.getClass}: ${b.toString}" })
  }

  implicit def renderEither[B,A](implicit r: Renderable[A]): Renderable[Either[B,A]] = new Renderable[Either[B,A]] {
    def render(ae: Either[B,A])(indent: Int): String = (ae.left map { b: B => s"Left: ${b.getClass}: ${b.toString}" }).fold(identity,{ a: A => s"Right(${r.render(a)(indent)})" })
  }

  // CONSIDER why do we need all four of these? But note that Cats has the same problem
  implicit def renderTraversable[A](implicit r: Renderable[A]): Renderable[Traversable[A]] = new Renderable[Traversable[A]] {
    def render(as: Traversable[A])(indent: Int): String = renderTraversable(as, max = Some(MAX_ELEMENTS), indent = indent)
  }

  implicit def renderIterable[A](implicit r: Renderable[A]): Renderable[Iterable[A]] = new Renderable[Iterable[A]] {
    def render(as: Iterable[A])(indent: Int): String = renderTraversable(as, max = Some(MAX_ELEMENTS), indent = indent)
  }

  implicit def renderSeq[A](implicit r: Renderable[A]): Renderable[Seq[A]] = new Renderable[Seq[A]] {
    def render(as: Seq[A])(indent: Int): String = renderTraversable(as, max = Some(MAX_ELEMENTS), indent = indent)
  }

  implicit def renderList[A: Renderable]: Renderable[List[A]] = new Renderable[List[A]] {
    def render(as: List[A])(indent: Int): String = renderTraversable(as, max = Some(MAX_ELEMENTS), indent = indent)
  }

  implicit def renderListList[A: Renderable]: Renderable[List[List[A]]] = new Renderable[List[List[A]]] {
    def render(as: List[List[A]])(indent: Int): String = renderTraversable(as, max = Some(MAX_ELEMENTS), indent = indent)
  }

  def renderTraversable[A: Renderable](xs: Traversable[A], bookends: String = "(,)", linear: Boolean = false, max: Option[Int] = None, indent: Int = 0): String = {
    val (p, q, r) = if (linear || xs.size <= 1) ("" + bookends.head, "" + bookends.tail.head, "" + bookends.last)
    else (bookends.head + nl[A](indent + 1), bookends.tail.head + nl[A](indent + 1), nl[A](indent) + bookends.last)
    elemsToString(xs, indent, p, q, r, max)
  }

  def elemsToString[A: Renderable](xs: Traversable[A], indent: Int, start: String, sep: String, end: String, max: Option[Int] = None): String = {
    val ar = implicitly[Renderable[A]]
    var first = true
    val b = new StringBuilder()
    b append start
    val z = max match {
      case Some(n) => xs take n
      case None => xs
    }
    for (x <- z) {
      if (first) {
        b append ar.render(x)(indent)
        first = false
      }
      else {
        b append sep
        b append ar.render(x)(indent)
      }
    }
    max match {
      case Some(n) => if (n < xs.size) b append s"$sep...[${xs.size - n} more elements]"
      case None =>
    }
    b append end
    b.toString()
  }

  /**
    * Method to insert an indented (tabbed) newline
    *
    * @return a string which is a suitable replacement for newline character
    */
  def nl[A: Renderable](indent: Int): String = "\n" + implicitly[Renderable[A]].tab(indent)

  /**
    * This is the default maximum number of elements that will be rendered by a Traversable (see renderTraversable).
    */
  val MAX_ELEMENTS = 10
}

object RenderableSyntax {

  implicit class RenderableOps[A](a: A) {
    def render(implicit r: Renderable[A]): String = r.render(a)()

    def render(indent: Int)(implicit r: Renderable[A]): String = r.render(a)(indent)
  }

  implicit class RenderableOpsList[A](as: List[A]) {
    import Renderable._
    def render(implicit r: Renderable[A]): String = renderTraversable(as)
    def render(indent: Int)(implicit r: Renderable[A]): String = renderTraversable(as, indent=indent)

//    def render(indent: Int)(implicit r: Renderable[A]): String = r.render(a)(indent)
  }

}

//case class Prefix(s: String) {
//  override def toString: String = {println(s"s is '$s'"); s}
//}
//
//object Prefix {
//
//  /**
//    * The default tab method which translates indent uniformly into that number of double-spaces.
//    *
//    * @param indent the number of tabs before output should start on a new line.
//    * @return a String of white space.
//    */
//  implicit def tab(indent: Int): Prefix = Prefix(" " * indent * 2)
//}
