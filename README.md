# LaScala
This package, developed by Phasmid Software and Dataflowage Inc. provides two different capabilities:

* A very basic set of functional programming methods to complement what's in the standard Scala library including:
** map2, map3, etc.
** lift2, lift3, etc.
* a rules parser together with a set of Predicates;
* a "Value" type which can be converted to several type classes:
  * Valuable (similar to Numeric);
  * Orderable ;
  * Incrementable;
* a simple Rational type;
* a set of self-fulfilling Cache classes;
* a set of parsers which will take a CSV file and yield a Stream of tuples or maps;
* a suite of tree data structures;
* a three-valued-logic module (Kleenean);
* a generic utility for implementing tail-recursive methods (Recursion);
* a utility to deal with command-line arguments (Args);
* a smart logger (SmartLogger);
* a simple "spy" utility for debugging via println statements (Spy);
* various other utilities.

Please contact us at [Dataflowage](http://www.dataflowage.com) if you are interested in receiving or providing consulting work in Spark, or any Big Data-related work.
Also, if you are interested in contributing to LaScala.

The functional programming definitions arose from teaching the class "Big Data Systems Engineering using Scala"
at Northeastern University College of Engineering Graduate School.

They (the definitions) don't claim to be anything very special.
No doubt it would be better to use Scalaz or Shapeless but if you only want a few
of the more basic definitions for things like map2, lift, sequence, etc. then this should do the job.

The rules parser, begun by Phasmid, has been further developed by Dataflowage.
Please contact us if you are interested in developing the rules -- or any Spark-, or Big Data-related work.
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

# Note on Scala Versions

LaScala is currently supported for 2.11 only. See the _build.sbt_ file for details on how to transform it to run with Scala 2.10
(although parts of the code simply cannot run and need to be removed).

I have not built LaScala with 2.12 at the present time.