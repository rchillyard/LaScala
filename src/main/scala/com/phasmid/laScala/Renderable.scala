package com.phasmid.laScala

import scala.language.implicitConversions

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
    * @param indent the number of "tabs" before output should start on a new line.
    * @param tab    an implicit function to translate the tab number (i.e. indent) to a String of white space.
    *               Typically (and by default) this will be uniform. But you're free to set up a series of tabs
    *               like on an old typewriter where the spacing is non-uniform.
    * @return a String that, if it has embedded newlines, will follow each newline with (possibly empty) white space,
    *         which is then followed by some human-legible part of *this*.
    */
  def render(indent: Int = 0)(implicit tab: Int => Prefix): String
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
