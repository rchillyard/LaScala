package com.phasmid.laScala

import com.phasmid.laScala.OldRenderable.MAX_ELEMENTS
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
  def tab(x: Int): Prefix = Prefix(" " * x)

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
    def render(s: Scalar)(indent: Int): String = s.toString
  }

  implicit val traversableRenderable: Renderable[Traversable[_]]  = new Renderable[Traversable[_]] {
    def render(xs: Traversable[_])(indent: Int): String = OldRenderableTraversable(xs, max = Some(MAX_ELEMENTS)).render(indent)
  }

}

object Renderable {
  def render[A](a: A)(indent: Int = 0)(implicit p: Renderable[A]): String = p.render(a)(indent)

  implicit def renderOption[A](implicit r: Renderable[A]): Renderable[Option[A]] = new Renderable[Option[A]] {
    def render(ao: Option[A])(indent: Int): String = ao map { x => r.render(x)(indent) } getOrElse "None"
  }

  implicit def renderTry[A](implicit r: Renderable[A]): Renderable[Try[A]] = new Renderable[Try[A]] {
    def render(ay: Try[A])(indent: Int): String = (ay map { a: A => r.render(a)(indent) }).recover { case t => t.getLocalizedMessage }.get
  }

  // CONSIDER eliminating this
  implicit def renderIterable[A](implicit r: Renderable[A]): Renderable[Iterable[A]] = new Renderable[Iterable[A]] {
    def render(a: Iterable[A])(indent: Int): String = (a map { x: A => r.render(x)(indent) }).mkString
  }

  implicit def renderSeq[A](implicit r: Renderable[A]): Renderable[Seq[A]] = new Renderable[Seq[A]] {
    def render(a: Seq[A])(indent: Int): String = {
      //      val sy: Seq[String] = a map { x: A => r.render(x)(indent) }
      // TODO what's this about reflective calls?
      RenderableInstances.traversableRenderable.render(a)(indent)
    }
  }
}

object RenderableSyntax {

  implicit class RenderableOps[A](a: A) {
    def render(implicit r: Renderable[A]): String = r.render(a)()

    def render(indent: Int)(implicit r: Renderable[A]): String = r.render(a)(indent)
  }

}