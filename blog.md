Type Classes in Scala
=====================

Type classes in Scala are one of those scary concepts to be used only by the cognoscenti, right?
Well, no, actually.
In reality, they are quite simple.

As usual, Daniel Westheide has some good things to say in his Neophyte's Guide to Scala: http://danielwestheide.com/blog/2013/02/06/the-neophytes-guide-to-scala-part-12-type-classes.html
Because his explanation is so good, I won't go into as much detail here as I might otherwise.

The somewhat confusingly-named Type Class is really just a different sort of trait.
If we have a Type Class "C" and a type "T" which is a member of "C", then "T" is required to implement the operations
that "C" requires.
But unlike the relationship between a trait and its implementations which is based on the polymorphism
inherent in object-oriented programming, the relationship between a type class and its members is more _ad hoc_ and,
in practice, relies on the "implicits" mechanism of Scala.

So, let's work on a practical example.
I was working on a rule parser.
This reads rules (as Strings) in the form of, for instance,

`feels cold: t < 0 or t < 5 and w > 10
`

where, in this case, t is temperature (in Celsius) and
w is wind speed in km/h.
