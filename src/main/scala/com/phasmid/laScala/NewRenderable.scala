package com.phasmid.laScala

import com.phasmid.laScala.Renderable.MAX_ELEMENTS
import com.phasmid.laScala.values.Scalar

import scala.util.Try

trait NewRenderable[A] {
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

object NewRenderableInstances {
  implicit val stringNewRenderable: NewRenderable[String] = new NewRenderable[String] {
    def render(w: String)(indent: Int): String = w replaceAll("""\n""", tab(indent).toString)
  }

  implicit val intNewRenderable: NewRenderable[Int] = new NewRenderable[Int] {
    def render(x: Int)(indent: Int): String = x.toString
  }

  implicit val doubleNewRenderable: NewRenderable[Double] = new NewRenderable[Double] {
    def render(x: Double)(indent: Int): String = x.toString
  }

  implicit val scalarNewRenderable: NewRenderable[Scalar] = new NewRenderable[Scalar] {
    def render(s: Scalar)(indent: Int): String = s.toString
  }

  implicit val traversableNewRenderable: NewRenderable[Traversable[_]]  = new NewRenderable[Traversable[_]] {
    def render(xs: Traversable[_])(indent: Int): String = RenderableTraversable(xs, max = Some(MAX_ELEMENTS)).render(indent)
  }

}

object NewRenderable {
  def render[A](a: A)(indent: Int = 0)(implicit p: NewRenderable[A]): String = p.render(a)(indent)

  implicit def renderOption[A](implicit r: NewRenderable[A]): NewRenderable[Option[A]] = new NewRenderable[Option[A]] {
    def render(ao: Option[A])(indent: Int): String = ao map { x => r.render(x)(indent) } getOrElse "None"
  }

  implicit def renderTry[A](implicit r: NewRenderable[A]): NewRenderable[Try[A]] = new NewRenderable[Try[A]] {
    def render(ay: Try[A])(indent: Int): String = (ay map { a: A => r.render(a)(indent) }).recover { case t => t.getLocalizedMessage }.get
  }

  // CONSIDER eliminating this
  implicit def renderIterable[A](implicit r: NewRenderable[A]): NewRenderable[Iterable[A]] = new NewRenderable[Iterable[A]] {
    def render(a: Iterable[A])(indent: Int): String = (a map { x: A => r.render(x)(indent) }).mkString
  }

  implicit def renderSeq[A](implicit r: NewRenderable[A]): NewRenderable[Seq[A]] = new NewRenderable[Seq[A]] {
    def render(a: Seq[A])(indent: Int): String = {
      //      val sy: Seq[String] = a map { x: A => r.render(x)(indent) }
      // TODO what's this about reflective calls?
      NewRenderableInstances.traversableNewRenderable.render(a)(indent)
    }
  }
}

object NewRenderableSyntax {

  implicit class NewRenderableOps[A](a: A) {
    def render(implicit r: NewRenderable[A]): String = r.render(a)()

    def render(indent: Int)(implicit r: NewRenderable[A]): String = r.render(a)(indent)
  }

}