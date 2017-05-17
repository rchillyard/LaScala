# LaScala
This package, developed by Phasmid Software and Dataflowage Inc. provides a smorgasbord of functional and general
utility libraries:

* A set of functional programming methods to complement what's in the standard Scala library including:
** map2, map3, etc.
** lift2, lift3, etc.
* (V1.0.1) a set of enhanced classes for composing functions together in such a way that they can be rendered;
* a set of generic Tree-traversal classes, including an MPTT (modified pre-order tree traversal) class;
* a rules parser together with a set of Predicates;
* a "Value" type which can be converted to several type classes:
  * Valuable (similar to Numeric);
  * Scalar;
  * Orderable ;
  * Incrementable;
* a Rational type;
* a set of self-fulfilling Cache classes;
* a set of parsers which will take a CSV file and yield a Stream of tuples or maps;
* a three-valued-logic module (Kleenean);
* a generic utility for implementing tail-recursive methods (Recursion);
* a utility to deal with command-line arguments (Args);
* a smart logger (SmartLogger);
* a simple "spy" utility for debugging via println statements (Spy);
* various other utilities.

## Revision History

Version 1.0.1 is almost ready to be frozen. It has the following enhancements/fixes:
* updates/fixes to the Renderable trait and classes which support more general Renderables,
such as Traversable, Option, Try, Either -- these are available via implicits;
* a pair of traits called RenderableFunction and Closure -- these are used to enhance the ability
to debug complex systems of functions (which are otherwise more or less opaque);

As of Jan 4th 2017, version 1.0.0 has been frozen (no more 1.0.0-SNAPSHOT).
Future revisions will be via a Development Branch and Pull Request.

## Notes

Please contact us at [Dataflowage](http://www.dataflowage.com) if you are interested in receiving or
providing consulting work in Spark, or any Big Data-related work.
Also, if you are interested in contributing to LaScala.

The functional programming definitions arose from teaching the class "Big Data Systems Engineering using Scala"
at Northeastern University College of Engineering Graduate School.

They (the definitions) don't claim to be anything very special.
No doubt it would be better to use Scalaz or Shapeless but if you only want a few
of the more basic definitions for things like map2, lift, sequence, etc. then this should do the job.

The rules parser, begun by Phasmid, has been further developed by Dataflowage.
There is no "engine" aspect to this rules parser: neither forward nor backward chaining.

The name, while obviously embedding the name of its implementation language, is also
in honor of the world's greatest opera house, [La Scala di Milano](https://en.wikipedia.org/wiki/La_Scala).
La Scala was the venue for the premieres of many of the greatest operas (in no particular order):

* Madama Butterfly
* La gazza ladra
* Nabucco
* Falstaff
* Andrea Chenier
* Norma
* Turandot
* Otello

# Note on Scala Binary Versions

LaScala (latest version of 1.0.0-SNAPSHOT and all later versions) is cross-built for 2.11 and 2.10. I have not built LaScala with 2.12 at the present time.