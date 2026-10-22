![Sonatype Central](https://maven-badges.sml.io/sonatype-central/com.phasmidsoftware/lascala_3/badge.svg?color=blue)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/8a78af8cecb64a25b509a0ab5109e7c0)](https://app.codacy.com/gh/rchillyard/LaScala/dashboard?utm_source=gh&utm_medium=referral&utm_content=&utm_campaign=Badge_grade)
[![CircleCI](https://dl.circleci.com/status-badge/img/gh/rchillyard/LaScala/tree/master.svg?style=shield)](https://dl.circleci.com/status-badge/redirect/gh/rchillyard/LaScala/tree/master)
![GitHub Top Languages](https://img.shields.io/github/languages/top/rchillyard/LaScala)
![GitHub](https://img.shields.io/github/license/rchillyard/LaScala)
![GitHub last commit](https://img.shields.io/github/last-commit/rchillyard/LaScala)
![GitHub issues](https://img.shields.io/github/issues-raw/rchillyard/LaScala)
![GitHub issues by-label](https://img.shields.io/github/issues/rchillyard/LaScala/bug)
[![API Docs](https://javadoc.io/badge2/com.phasmidsoftware/lascala_3/javadoc.svg)](https://javadoc.io/doc/com.phasmidsoftware/lascala_3)

# LaScala
This package, developed by Phasmid Software and Dataflowage Inc. provides a smorgasbord of functional and general
utility libraries:

* A set of functional programming methods to complement what's in the standard Scala library including:
  * map2, map3, etc.
  * lift2, lift3, etc.
* (V1.0.1) a set of enhanced classes for composing functions together in such a way that they can be rendered;
* a set of generic Tree classes (with traversal);
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
* a Renderable type class which gives a prettier and better structured textual representation of objects than toString (see below);
* various other utilities, including a Version type.

## Installation

```scala
libraryDependencies += "com.phasmidsoftware" %% "lascala" % "2.1.0"
```

LaScala 2.0 is for Scala 3 only. It is compiled with Scala 3.3, so it can be used from any Scala 3.3 or later (it is also tested on 3.9).
* For Scala 2.13, use version 1.1.0 (which is cross-built for 2.13 and 3).
* For Scala 2.10, 2.11 or 2.12, use version 1.0.x.

Version 2.0 has breaking changes: see [Migrating from 1.x](#migrating-from-1x).

## Renderable

`Renderable[A]` is a type class for rendering an `A` in a human-legible way, with nested structures indented.
A case class, sealed trait or enum can simply write `derives Renderable`:

```scala
import com.phasmidsoftware.lascala.Renderable

case class Player(name: String, rating: Double, games: List[Int]) derives Renderable

Player("Robin", 1850.5, List(1, 2)).render
// Player(
//   name:"Robin",
//   rating:1850.5,
//   games:(
//     1,
//     2
//   )
// )
```

No import is needed to call `render` on a type with its own instance (as here, or `Scalar`, `Tree`, `Version`, `Closure` and `RenderableFunction`).
For the built-in types, import the instances:

```scala
import com.phasmidsoftware.lascala.Renderable.given

List(Some(1), None).render                // (\n  Some(1),\n  None\n)
Map("x" -> 1).render                      // ("x"->1)
("x", 42).render                          // Tuple2("x",42)
```

* There are instances for the basic types (`Int`, `Double`, `String`, etc.), `Option`, `Try`, `Either`, `Map`, tuples, and every collection.
  A String is quoted, so that it stands out within a structure.
* A collection with at least two elements has one element per line; at most 10 elements are shown, followed by `...[n more elements]`.
* A type with no instance does not compile. To render a type with its `toString`, write `given Renderable[Foo] = Renderable.fromToString`.
* Sealed traits and enums (including recursive ones) render the case. A case's own given instance is used if it has one.

To write an instance yourself, implement `render(a, indent)`, where `indent` is the number of tabs for any lines after the first:

```scala
import com.phasmidsoftware.lascala.{Layout, Renderable}

case class Temperature(celsius: Double)

object Temperature {
  given Renderable[Temperature] = new Renderable[Temperature] {
    def render(t: Temperature, indent: Int)(using Layout): String = s"${t.celsius}°C"
  }
}

Temperature(21.5).render                  // 21.5°C
```

The layout (the tab, the maximum number of elements, and the bookends of a collection) comes from a `Layout`, whose default needs no import.
To change it, declare a given `Layout`. A given applies to the whole of its block, including any lines before it, so put it in an inner block:

```scala
locally {
  given Layout = Layout(tab = Layout.uniform(4), bookends = "[;]")
  List(1, 2).render                       // [\n    1;\n    2\n]
}
```

### Notes on Scala 3

* _RenderableFunction_ with call-by-name parameters (e.g. _def f(s: => String)_):
  Scala 3 cannot infer a by-name type for a type parameter, so passing such a method to _RenderableFunction.apply_ does not compile.
* _Spy_: if you define your own implicit _spyFunc_ in a scope where you have also written _import Spy.\__,
  Scala 3 prefers the imported default (the logger-based one).
  Write _import Spy.{spyFunc => \_, \_}_ instead.

## Migrating from 1.x

| 1.x | 2.0 |
|---|---|
| Scala 2.13 and 3 | Scala 3 only (3.3 or later) |
| `trait Renderable` with `render(indent: Int = 0)(implicit tab: Int => Prefix)` | type class `Renderable[A]`: `a.render`, or `Renderable[A].render(a, indent)` for a starting indent |
| `x.render()` | `x.render` (no parentheses) |
| `x.render(indent)` | `Renderable[X].render(x, indent)`. **Beware:** `x.render(indent)` still compiles when `x` has an instance, but it means `x.render.apply(indent)`, the `Char` at position `indent`, which may then cause a confusing type error or none at all |
| `class Foo extends Renderable { def render(indent: Int)(implicit tab: Int => Prefix) = … }` | `given Renderable[Foo]` in `object Foo` (or `derives Renderable` for a case class) |
| `RenderableCaseClass(x)` | `case class X(…) derives Renderable` |
| `RenderableTraversable`, `RenderableOption`, `RenderableTry`, `RenderableEither`, `RenderableProduct` and the implicit conversions in `object Renderable` | instances for `Iterable`, `Option`, `Try`, `Either` and tuples (`import Renderable.given` for `.render` on these) |
| `RenderableTraversable(xs, bookends, max = …)` | `given Layout = Layout(bookends = …, maxElements = …)` |
| `Prefix`, and an implicit `tab: Int => Prefix` | `Layout(tab = …)`; `Layout.uniform(n)` for n spaces per tab |
| `Renderable.MAX_ELEMENTS` | `Layout.MAX_ELEMENTS` |
| `Renderable.renderElem(x: Any, …)` (chosen at run time, with `toString` as a fallback) | the instance is chosen at compile time; there is no fallback (`Renderable.fromToString` to opt in) |
| A String inside a structure: unquoted in a `Tree`, quoted elsewhere | always quoted |
| `IndexedLeaf` of a value which is not `Renderable`: `value [l:r]` | rendered like any other leaf |
| `Punctuation(x: String)` | `Punctuation(val x: String)` |
| `RenderableFunction` and `Closure` tests with call-by-name parameters (Scala 2 only) | removed |

## Revision History
### Changes in V2_1_0
* The package _com.phasmidsoftware.lascala.sort_ (_Comparer_, _Comparison_, _Different_, _Same_, _Sorted_, _Sortable_) is deprecated,
  and will be removed in 3.0. It is an early version of what became the [Comparer](https://github.com/rchillyard/Comparer) library:
  use that instead (_libraryDependencies += "com.phasmidsoftware" %% "comparer" % "2.0.0"_).
  Note that Comparer's API differs: its _Comparer[T]_ is curried (_T => T => Comparison_), and its _Comparison_ is an enum of _Less_, _More_ and _Same_.
### Changes in V2_0_0
* Scala 3 only (compiled with 3.3.8; also tested on 3.9.0)
* _Renderable_ is now a type class, with instances for the common types and derivation (_derives Renderable_)
  for case classes, sealed traits and enums; _Layout_ replaces _Prefix_ and the implicit tab function.
  See [Migrating from 1.x](#migrating-from-1x).
* _Scalar_, _Node_/_Tree_, _Version_, _Closure_ and _RenderableFunction_ have _Renderable_ instances in their companion objects
* Removed: the _Renderable_ trait and its wrapper classes, _Prefix_, _RenderableCaseClass_ and _CaseClasses_
* _Version.parse_ (and _parseLong_) no longer hangs on a version with three or more components, such as 1.0.11, or with a component it cannot parse (which now yields None)
### Changes in V1_1_0
* The organization (group id) is now _com.phasmidsoftware_ (formerly _com.phasmid_), like the other Phasmid libraries:
  _libraryDependencies += "com.phasmidsoftware" %% "lascala" % "1.1.0"_.
* The packages are renamed to match: _com.phasmid.laScala_ is now _com.phasmidsoftware.lascala_
  (so, for example, _import com.phasmid.laScala.fp.FP.\__ becomes _import com.phasmidsoftware.lascala.fp.FP.\__).
* Cross-built for Scala 2.13 and 3 (Scala 2.10, 2.11 and 2.12 are no longer supported); sbt 1.11.7
* Current dependencies: scala-parser-combinators 2.4, scala-logging 3.9, ScalaTest 3.2 (tests)
* API changes needed for Scala 2.13 and 3:
    * _Quicksort.qsort_ takes an implicit _Factory_ (formerly _CanBuildFrom_)
    * _Rational_, _Fuzzy_ and _FuzzyParser.FractionalInt_ implement _Numeric.parseString_
    * _RenderableFunction.partiallyApply(t)_ no longer has an (unused) _ClassTag_ context bound
    * Some methods that were called with explicit evidence now declare an explicit implicit parameter list instead of a context bound
      (_Exact_, _Exact.fromInt_, _Fuzzy.apply_, _Orderable.comparisonTyped_, _Rational.convertToM_, _Sorted.verify_,
      _RenderableFunction.getPartialApplicationFunction_)
    * The extractors _Branch_, _AbstractBinaryTree_ and _IndexedNode_ accept any _Node_
    * _FP.zippy_ returns strict collections
* From the (superseded) V1_0_11 master branch:
    * _Benchmark_: _n.times(f)_ now returns a _Benchmark[A]_ (formerly a _Long_); its _nanos_ is the average nano-seconds per run, after a warm-up of 20% of the runs
    * _Rational_: _toBigDecimal_ returns a _Try[BigDecimal]_ (a _Failure_ for infinity, instead of throwing); _toDouble_ yields _Double.NaN_ for infinity; _reciprocal_ is a synonym for _invert_
### Changes in V1_0_8
* RenderableFunction, Closure, etc.
    * forced Seq type to be List for parameters of any serializable classes
### Changes in V1_0_7
* RenderableFunction, Closure, etc.
    * forced Seq type to be List for parameters of any serializable classes
### Changes in V1_0_6
* Tree:
    * refactored traits/classes for cleaner code;
    * added GeneralKVTreeWithScaffolding which uses a hashmap to find existing nodes during the building of a tree;
* FP:
    * added nextOption
    * added foldLeftShort
### Changes in V1_0_5
* Tree: minor change in render method
### Changes in V1_0_4
* Tree: better logic in addNode; refactored by separating out StructuralNode and StructuralTree types
### Changes in V1_0_3
* improvements to Version: now we use sbt-release and sbt-buildinfo
### Changes in V1_0_2
* FP: added map2g and flatMap2g; MPTT: added containsConditional
### Changes in V1_0_1
* a collaborative pair of classes: RenderableFunction and Closure which are designed to all for the development,
debugging and deployment of AST-type parsing, for example the parsing of case clauses within SQL;
* updates/fixes to the Renderable trait and classes which support more general Renderables,
such as Traversable, Option, Try, Either -- these are available via implicits;
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
