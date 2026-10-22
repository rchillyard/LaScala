# LaScala: code review and plan (October 2026)

This is a review of LaScala's own code, as it stands after the move to Scala 3 (2.0.0). It isn't a review of the migration.

It has three parts:
- an assessment of the code's quality
- the overlap with libraries that didn't exist, or weren't mature, when LaScala was written (2016–2018), especially Typelevel's, and your own later libraries
- a plan: what to deprecate in 2.x, what to remove in 3.0, and what to keep

The figures are for `master` at the 2.0.0 release (commit `236d6c4`). The library versions are the latest on Maven Central for Scala 3 in October 2026, and each one was checked there. Every class this document names was confirmed in the published jar.

---

## 1. Summary

The code is **uneven, rather than bad**. The ideas are often good, and the tests are unusually strong. But a lot of it was never finished, pruned or carried through.

It reads like several years of teaching and experimenting in one repository. Comparer is the contrast: it shows what one of those ideas looks like when it's carried all the way through.

The most useful next step is **pruning** rather than polishing. Much of what LaScala provides now exists, better maintained, in Cats, Cats Effect, Spire or your own libraries (Comparer, Number, Flog, TableParser). LaScala 3.0 could be much smaller and much better.

---

## 2. Strengths

- **The tests are real.** There are over 800 tests with precise assertions. Their expected strings survived two Scala versions and the complete `Renderable` redesign byte-for-byte, and that is what made the migration safe.
- **The ideas hold up.** `RenderableFunction`/`Closure` (functions that can show themselves as an expression tree), the self-fulfilling caches, three-valued logic (`Kleenean`), and fuzzy numbers with probabilities are all interesting. Several were ahead of what the standard library offered at the time.
- **The Scaladoc is generous**, and the intent of most methods is clear.

## 3. Weaknesses

| Measure (main code: 56 files, 11,087 lines) | Count |
|---|---|
| `asInstanceOf` casts | 55 |
| Methods still `???` | 7 |
| `TODO` / `CHECK` / `XXX` / `CONSIDER` / `FIXME` comments | 139 |
| `implicit def`s | 28 |
| Files that enable `implicitConversions` | 18 |
| `var`s | 11 |
| Ignored tests (in the Scala 3 build) | 17 |

- **It's unfinished in places.**
  - `Gaussian`'s probability methods, and `getParent` and `nodesAlike` in a tree builder, are `???`.
  - `MPTT` exists only in the test sources.
  - The call-by-name tests for `RenderableFunction` and `Closure` were ignored from 2.12 onwards, with "investigate why this doesn't work"; 2.0 removed them.
  - Some comments say as much: `tree/Tree.scala:976` "NOTE this looks totally wrong!!", and `values/FuzzyParser.scala:34` "CHECK that the following cast is OK. I don't really think it is."
- **Types often give way at run time.**
  - The 1.x `Renderable` chose how to render each value at run time, by matching on `Any`. 2.0 replaced that.
  - The tree code casts its way around variance (`y.asInstanceOf[Tree[B]] // CHECK this is OK`).
- **It leans heavily on implicits.** There are implicit conversions, an implicit function for tabs (`Int => Prefix`, now gone), an implicit `Int` (`FP.limit`), and `Spy.spyFunc` as an implicit conversion method. That made behaviour depend on what's imported, and it is exactly what broke under Scala 3: `Spy` silently picked the wrong function. Implicit values of primitive or function type are a hazard (playbook §5).
- **It had latent bugs that nothing tested.** `Version.parse` looped forever on any three-part version; this is fixed in 2.0.0, but 1.1.0 still has it. `FP.zippy` gave wrong answers on 2.13 because it reused iterators; fixed in 1.1.0.
- **There's global mutable state.** `Spy.spying` and `BooleanScalar`'s default format are global `var`s. `MutableGenericIndexedTree` has `var` fields. `CacheMap` extends `mutable.HashMap`, which has been deprecated for inheritance since 2.13.
- **It's a kitchen sink, with duplicates.**
  - Three type classes express "has a successor": `Incrementable`, `Sequential` and `Recursion.Counter`, whose own comment says "Note: this is similar to Incrementable".
  - `Orderable` sits alongside `sort.Comparer`, whose comment says "Probably we should merge the two concepts".
  - `sort` duplicates the Comparer library; it's deprecated since 2.1.0.
  - Its own `Monoid` is marked "experimental".
  - Comparer has a second `Kleenean`.
- **The process was weak too.** CI was broken from about 2018 until October 2026. Coverage has never been measured. `FPSpec` depends on the network (google.com), and `BenchmarkSpec` on the speed of the machine. About 100 compiler warnings about Scala 2 idioms remain.

---

## 4. Overlap with libraries that exist now

Most of these replacements are a **deprecation that points users elsewhere**, as for `sort` in 2.1.0. That needs **no new dependency** in LaScala. A dependency is only needed if code that stays in LaScala wants one of these abstractions internally.

**Key:**
- **Replace:** deprecate and point users to the alternative.
- **Retire:** deprecate, with no direct alternative needed: it's too small, too specialised, or the standard library covers it.
- **Keep:** it stays in LaScala.

### 4.1 Functional programming (`fp`)

| LaScala | What it does | Alternative (Scala 3, October 2026) | Recommendation |
|---|---|---|---|
| `FP.map2`/`map3`/`map7`, `lift`/`lift2`/`lift3`/`lift7`, `sequence` (7 overloads), `zip`, `flatten`, `toOption`, `optionToTry`, `liftFuture`, `asFuture` | Applicative and traversal helpers for `Option`/`Try`/`Either`/`Future`, as arity families | **Cats** `cats-core_3` 2.13.0: `(a, b).mapN(f)` (`Apply`/`Semigroupal`), `xs.sequence` and `xs.traverse(f)` (`Traverse`), `Validated` for accumulating errors. The standard library covers `Option.zip`, `Try.toOption`, `Future.sequence` and `Future.traverse`. | **Replace.** Arity families are what the playbook (§6) says to remove. |
| `FP.untupled`, `uncurried2`/`3`/`7`, `invert2`…`invert6` | Function plumbing, as arity families | Standard library: `Function.tupled`/`untupled`/`uncurried`, `f.curried`. Scala 3 tuples generalise the rest. | **Retire.** |
| `Lazy` | A lazily evaluated value | Standard library `lazy val` and by-name parameters; **Cats** `Eval` (`Later`, `Always`, `Now`) | **Replace.** |
| `TryWith` | Try with automatic resource closing | Standard library `scala.util.Using` (2.13+); **Cats Effect** `Resource` (3.7.1) | **Replace.** |
| `Trial`, `Match`, `LiftTrial` | Chaining `Try` alternatives | Standard library `Try.orElse`, `PartialFunction.orElse`; **Cats** `orElse`/`Alternative` | **Retire.** |
| `Spy`, `SmartLogger` | Debug logging that returns the value logged | **Flog** (yours) `flog_3` 1.0.15; Typelevel **log4cats** `log4cats-core_3` 2.8.0 | **Replace** with Flog: it's the successor to this idea. |
| `Benchmark` | `n.times(f)` timing | **sbt-jmh** (JMH) for real measurement | **Retire.** |
| `Args` | A cursor over a `List[Any]` of arguments | For real command lines: **decline** `decline_3` 2.6.2 (Cats-based), or scopt | **Retire** (Darwin 1.0.10 used it). |
| `Streamer` | A mutable, read-once stream | Standard library `Iterator` | **Retire** (Darwin 1.0.10 used it). |
| `NamedFunction`, `NamedFunction0`…`5`, `RenderableFunction`, `Closure`, `FunctionString` | Functions that carry a printable name, and expression trees of them | No direct equivalent. Number's `expression` module covers symbolic expressions over numbers, which is related but not the same. | **Keep, or move to its own library.** This is LaScala's most original idea, but it uses arity families (`apply` up to 9 parameters) and a lot of casting, and it can't handle call-by-name parameters in Scala 3. If kept, generalise it with Scala 3 tuples, as Comparer did. Darwin 1.0.10 used `Named`/`NamedFunction`. |
| `HasKey` | A type class for something with a `String` key | It's tiny; any user can define it. | **Retire**, unless the tree code keeps needing it. |

### 4.2 Values and numbers (`values`, top level)

| LaScala | What it does | Alternative | Recommendation |
|---|---|---|---|
| `Rational`, `FiniteIntegral`, `Rational_Cross` | Exact rationals over `Int`/`Long`/`BigInt` | **Number** (yours) `number-core_3` 1.10.5: `core.inner.Rational`; **Spire** `spire_3` 0.18.0: `spire.math.Rational` and `spire.math.Integral` | **Replace** with Number's `Rational`: it's yours and maintained. |
| `Fuzzy`, `FuzzyParser`, `Probability` | Fuzzy numbers (`Exact`, `Bounded`, `Gaussian`) and probabilities | **Number** `number-core_3` 1.10.5: `Fuzziness` with `Box` and `Gaussian` shapes, `AbsoluteFuzz`, `RelativeFuzz`, `FuzzyNumber` | **Replace** with Number. LaScala's version has 5 of its 7 `???`s and the doubtful cast. |
| `Orderable` | Ordering, plus parsing from a `String` | **Cats** `cats.kernel.Order`; **Comparer** `comparer_3` 2.0.0 | **Replace.** The parsing half belongs with `Valuable`. |
| `Incrementable`, `Sequential`, `Recursion.Counter` | Three type classes for "has a successor" | **Cats** `cats-kernel_3` 2.13.0: `cats.kernel.PartialNext` and `BoundedEnumerable` | **Replace** all three. Darwin 1.0.10 used `Incrementable` and `Sequential`. |
| `Monoid`, `Additive` | An experimental monoid | **Cats** `cats.kernel.Monoid` | **Replace.** |
| `Tuples` (`Tuple0`, `Product0`, `SeqProduct`) | Scala 2 workarounds for an empty tuple | Scala 3 `EmptyTuple` and generic tuples | **Replace** (`RenderableFunction` uses `Tuple0`, so this goes with it). |
| `Scalar`, `Value`, `Valuable`, and their subclasses | Dynamically typed values parsed from text (`IntScalar`, `DateScalar`, `QuotedStringScalar`, etc.) | Nothing quite the same. JSON ASTs such as circe's `Json` are similar in spirit. | **Keep.** `lab-parser` uses `Scalar`, `DateScalar` and `QuotedStringScalar`. |
| `Kleenean`, `Maybe` | Three-valued logic, used by the tree code | **Comparer** `comparer_3` 2.0.0: `Kleenean` (an enum: `Truth`, `Maybe`); or simply `Option[Boolean]` | **Decide.** Adopting Comparer's would make LaScala depend on Comparer, which you've chosen not to do for `sort`. If the trees are retired (§4.4), retire this too. |
| `Version` | Versions such as 1.0.11 | Nothing worth a dependency | **Keep** (small; Darwin 1.0.10 used it). |

### 4.3 Randomness, recursion, caching

| LaScala | What it does | Alternative | Recommendation |
|---|---|---|---|
| `RNG`, `LongRNG`, `DoubleRNG`, `GaussianRNG`, `RandomState`, `UniformDouble`, `Shuffle` | Pure, functional random numbers | **Cats** `cats.data.State` with `scala.util.Random` (a pure RNG is the textbook `State` example); **Cats Effect** `cats.effect.std.Random` (3.7.1); ScalaCheck `Gen` for tests | **Replace.** Darwin 1.0.10 used `RNG`, `LongRNG` and `Shuffle`. |
| `Recursion` (`recurse`, `countRecurse`, `genericRecurse`), `Factorial` | Generic tail-recursion helpers | Plain `@tailrec` methods; **Cats** `Monad.tailRecM`; `scala.util.control.TailCalls`; **droste** `droste-core_3` 0.10.0 for recursion schemes | **Retire.** |
| `cache.Cache` and the `FulfillingCache` and `ExpiringCache` family | Self-fulfilling caches, some with expiry | **Scaffeine** `scaffeine_3` 5.3.0 (Caffeine for Scala): loading and expiring caches. **Cats Effect** `Ref` and `MapRef` for a purely functional version. | **Replace.** Also, `CacheMap` extends `mutable.HashMap`, which has been deprecated for inheritance since 2.13. |
| `ActorLike`, `Agent`, `Queryable` | A façade for actor-like services | Pekko (or Akka), or Cats Effect | **Retire** (no known users). |
| `Quicksort` | A teaching example of `CanBuildFrom`/`Factory` | Standard library `sorted` | **Retire.** |
| `sort` (`Comparer`, `Comparison`, `Sorted`) | Comparisons and sorting | **Comparer** `comparer_3` 2.0.0 | **Done:** deprecated in 2.1.0, to be removed in 3.0. |

### 4.4 Parsing, rules and trees

| LaScala | What it does | Alternative | Recommendation |
|---|---|---|---|
| `ProductStream`, `CSV`, `TupleStream`, `Header`, `Ingestible`/`Ingest` | Reading CSV into tuples, maps or case classes | **TableParser** (yours) `tableparser-core_2.13` 1.5.1, not yet on Scala 3; **fs2-data-csv** `fs2-data-csv_3` 1.14.1 (Typelevel-affiliated). kantan.csv has no Scala 3 release. | **Replace** with TableParser once it's on Scala 3. Until then, deprecate, pointing to TableParser for 2.13 and fs2-data-csv for Scala 3. |
| `RuleParser`, `Rule`, `Predicate` (with `And`, `Or`, `GT`, …), `rpn.RPN`, `Comparand`, `Expression` | A rules parser, predicate combinators, and a reverse-Polish-notation evaluator | Parsing: **cats-parse** `cats-parse_3` 1.1.0 (Typelevel), as an alternative to scala-parser-combinators. Predicate combinators are `A => Boolean` with Cats' `Monoid`/`Semigroup` instances. No library offers the rules engine itself. | **Decide.** There are no known users. Either move it into its own small library, built on cats-parse, or retire it. |
| `Tree`, `Node`, `Branch`, `Leaf`, `KVTree`, `GeneralKVTree`, `Parent`, `TreeIndex`, `IndexedNode` (and `MPTT`, in the test sources) | General and key-value trees, with modified pre-order traversal (MPTT) | No Typelevel equivalent: **Cats** `cats-free_3` 2.13.0 has `cats.free.Cofree`, a rose tree; Scalaz has a `Tree`. **cats-collections** `cats-collections-core_3` 0.9.10 has a balanced tree (`AvlSet`), which is a different use. | **Decide.** `Tree.scala` alone is 1,365 lines, with most of the casts, two `???`s and the "totally wrong" comment. There are no known users. Retire it, or rewrite it as a small, typed rose tree. |
| `Renderable`, `Layout` (rewritten in 2.0) | Indented, structured rendering, with derivation | **Cats** `cats.Show`: single-line, no layout or indentation. **pprint** `pprint_3` 0.9.6 (Li Haoyi): generic pretty-printing of any value, with no instances to write. | **Keep.** It does more than `Show`, and unlike pprint it lets each type control its own rendering. Consider a `Show` instance derived from `Renderable` for interoperability. |

---

## 5. Known users

These decide what can simply go.

- **CSYE7200 `lab-parser`**, on LaScala 2.0.0: `values.Scalar`, `DateScalar`, `QuotedStringScalar`, `Renderable` and `Layout`.
- **Darwin**, on 1.0.10-SNAPSHOT (Scala 2.11, 2017): its imports are listed below. It's pinned to an old snapshot, so nothing here affects it unless it's ever migrated.
  - `fp`: `FP`, `Named`, `NamedFunction`, `NamedFunction3`, `Streamer`, `Args`
  - `values`: `Incrementable`, `Rational`
  - top level: `Version`, `Sequential`, `RNG`, `LongRNG`, `Shuffle`, `Prefix`, `Renderable`, `RenderableCaseClass`
  - tests: `MockLogger`

## 6. Plan

1. **2.1.0** (PR #18): deprecate `sort`. Done.
2. **2.2.0 (deprecations, no API breaks)**: deprecate everything marked Replace or Retire in §4, each with a message naming the replacement, as `sort`'s does. This needs no new dependencies. Also suppress the deprecation warnings in the affected specs, as was done for `sort`.
3. **Decide** the three open questions: `RenderableFunction`/`Closure`, the rules engine, and the trees. Each one is keep, move to its own library, or retire.
4. **3.0.0**: remove everything deprecated. What remains would roughly be:
   - `Renderable`
   - the `Scalar`/`Value` family
   - `Version`
   - whatever survives step 3

   Then give that remainder the Comparer treatment, before or with 3.0:
   - `given`/`using` throughout, and no implicit conversions or primitive givens
   - enums for the sealed hierarchies
   - no `asInstanceOf`, `???` or global `var`s
   - the indentation-syntax rewrite and `-Wunused:all`
   - measured coverage (aim for 100%, as Comparer does)
   - tests that depend on neither the network nor timing
5. **Each release**: update the README's migration table and revision history, and upload the docs on javadoc.io.

## Appendix: how the figures were measured

All were measured in `src/main/scala` at commit `236d6c4`:
- **Casts:** `grep -rho 'asInstanceOf'`
- **Stubs:** `grep -rhoE '= \?\?\?'`
- **Comment markers:** `grep -rhoE '\b(TODO|CHECK|XXX|CONSIDER|FIXME)\b'`
- **Implicits:** `grep -rhoE 'implicit def'`
- **`var`s:** `grep -rhoE '^\s*(private )?var |\bvar [a-z]'`
- **Ignored tests:** `grep -rhoE 'ignore should'` in `src/test`

The migration to Scala 3 added a few casts of its own (in `Renderable`'s derivation) and removed others, so the counts are close to, but not exactly, those of the 1.x code.
