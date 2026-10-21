# Renderable for LaScala 2.0: design proposal

Status: **accepted** (2026-10-21), with the recommended answer to each decision in §4. Being implemented on branch `V2_0_0` (from `V1_1_0`), following the plan in §5.

LaScala 2.0 would be Scala 3 only (see the Comparer playbook, §1). Its first piece of work is replacing the object-oriented `Renderable` trait with a type class, starting from the `Renderable[A]` that was on the old master branch (V1_0_11, dropped from 1.1.0).

---

## 1. What we have now

**1.x (`V1_1_0`): an OO trait.**
```scala
trait Renderable {
  def render(indent: Int = 0)(implicit tab: Int => Prefix): String
}
```
- Library types extend it: `Scalar`, `Node`/`Tree`, `BaseVersion`, `Closure`, `RenderableFunction`.
- Wrapper case classes add rendering to types that don't extend it: `RenderableTraversable`, `RenderableOption`, `RenderableTry`, `RenderableEither`, `RenderableProduct` and `RenderableCaseClass`. Implicit conversions to these wrappers are defined in `object Renderable`.
- `Renderable.renderElem(elem: Any, …)` decides how to render each element **at run time**, by matching on its class, and falls back to `toString`.
- `RenderableCaseClass` gets field names from `TypeTag` on 2.13 and from `productElementNames` on Scala 3. These are two different implementations.
- Indentation is supplied by an **implicit function** `Int => Prefix`. Scala 3 treats this badly: it's an implicit conversion in disguise, and the playbook (§5) says never to make a given of a function or primitive type.

**Old master (V1_0_11): a type class `Renderable[A]`.**
- `render(a: A)(indent: Int)`, with instances for `String`, `Int`, `Double`, `Scalar`, `Option`, `Try` and `Either`.
- There are **four** collection instances (`Traversable`, `Iterable`, `Seq`, `List`), plus `List[List[A]]`, because the type class is invariant. Its own comment asks why all four are needed.
- `tab` is a method on each instance, so the indentation style can't be chosen independently of the type.
- The syntax is in `RenderableSyntax` (implicit classes) and the instances in `RenderableInstances`, so users need two imports.
- There's no derivation, so case classes need hand-written instances.

## 2. Proposal

### 2.1 The type class

```scala
package com.phasmidsoftware.lascala

trait Renderable[A]:
  /** Render a, whose first line starts at the current position and whose later lines are indented by indent tabs. */
  def render(a: A, indent: Int)(using Layout): String

  extension (a: A) def render(using Layout): String = this.render(a, 0)

object Renderable:
  def apply[A](using r: Renderable[A]): Renderable[A] = r
  def fromToString[A]: Renderable[A] = …          // an explicit factory, never automatic
  inline def derived[T](using Mirror.Of[T]): Renderable[T] = …

  given intRenderable: Renderable[Int] = fromToString
  given stringRenderable: Renderable[String] = …  // quoted, as 1.x renders Strings inside structures
  // Long, Double, Boolean, BigInt, BigDecimal, Char, Unit, …
  given optionRenderable[A: Renderable]: Renderable[Option[A]] = …
  given tryRenderable[A: Renderable]: Renderable[Try[A]] = …
  given eitherRenderable[L: Renderable, R: Renderable]: Renderable[Either[L, R]] = …
  given iterableRenderable[A, CC[X] <: Iterable[X]](using Renderable[A]): Renderable[CC[A]] = …
  given mapRenderable[K: Renderable, V: Renderable]: Renderable[Map[K, V]] = …
  // tuple instances, used by derivation (§2.2)
```

**Layout replaces `Int => Prefix` and `MAX_ELEMENTS`:**
```scala
final case class Layout(tabWidth: Int = 2, maxElements: Option[Int] = Some(10)):
  def tab(indent: Int): String = " " * (indent * tabWidth)

object Layout:
  given default: Layout = Layout()
```
Because `Layout` lives in its own companion object, the default is always found without an import. A caller can override it locally with `given Layout = Layout(tabWidth = 4)`. That replaces 1.x's custom-tab mechanism without a function-typed given.

**One collection instance.** `iterableRenderable[A, CC[X] <: Iterable[X]]` covers `List`, `Vector`, `Seq`, `Set`, `LazyList` and so on, which removes master's four instances.

### 2.2 Derivation

`case class Player(name: String, rating: Double) derives Renderable` follows the playbook's §6 pattern:
- **Products:** `Mirror.ProductOf` gives the fields as a tuple. A by-name, lazy tuple instance renders each field, so a recursive type can use its own instance while it is still being defined.
- **Field names** come from `MirroredElemLabels`, at compile time and in declaration order. This replaces both `CaseClasses.caseClassParamsOf` (which uses `TypeTag`) and Scala 3's `productElementNames`, so `RenderableCaseClass` and the `scala-2.13`/`scala-3` source split are no longer needed.
- **Sums:** sealed traits and enums use `Mirror.SumOf` and `ordinal`. Each case uses its own given if it has one, and is derived otherwise.

### 2.3 Output format: kept from 1.x

The 1.x layout is kept, so the existing `RenderableSpec`, `TreeSpec` and `ClosureSpec` expectations can be ported unchanged (playbook §7: "keep each ported test's expected result").
- A collection with zero or one element renders on one line, `(x)`. A longer one gets one element per line, with the bookends `(`, `,` and `)`.
- Two spaces per tab.
- At most 10 elements, then `...[n more elements]`.
- A case class renders as `Name(` followed by `field:value` lines.

The one deliberate change is that a String inside a structure is quoted (`name: "Robin"`), as in 1.x's `renderElem`. Master's type class didn't quote.

### 2.4 Library types

Each class that extends the 1.x trait gets a named given in its own companion object instead, so users never need an import for it:

| 1.x | 2.0 |
|---|---|
| `trait Scalar extends Renderable` | `given Renderable[Scalar]` in `object Scalar` (keeps `renderFormatted`) |
| `sealed trait Node[+A] extends Renderable` | `given [A: Renderable]: Renderable[Node[A]]` in `object Node`. Values no longer fall back to `toString` (see D2). |
| `BaseVersion … with Renderable` | `given Renderable[Version[…]]` |
| `Closure`, `RenderableFunction` | givens in their companions |
| `RenderableTraversable`, `RenderableOption`, `RenderableTry`, `RenderableEither`, `RenderableProduct`, `RenderableCaseClass` | removed: replaced by the instances in §2.1 and by derivation |
| `Prefix`, `Prefix.tab` | removed: replaced by `Layout` |
| `Renderable.renderElem(elem: Any, …)` | removed: the instance is chosen at compile time |

### 2.5 What a user writes

```scala
import com.phasmidsoftware.lascala.Renderable

case class Player(name: String, rating: Double, games: List[Int]) derives Renderable

Player("Robin", 1850.5, List(1, 2)).render   // no import: the given is in Player's companion

import Renderable.given                      // needed only for .render on built-in types
List(Some(1), None).render
```

---

## 3. Verified in a prototype (Scala 3.3.8 and 3.9.0)

A scratch prototype (not in the repo) of §2.1–2.2 compiled and ran on both versions:
- `derives Renderable` on a case class, and on a recursive `enum Tree { case Leaf; case Node(left: Tree, value: Int, right: Tree) }`, gives the field names and nesting shown in §2.3.
- The single `iterableRenderable` instance resolves for `List`, `Vector`, `Seq[Option[Int]]` and `List[Player]`.
- `.render` on a type that uses `derives` needs **no import**. `.render` on `Int` or `List` needs `import Renderable.given`, which is enough on its own.
- Overriding with `given Layout = …` works, but in a block it applies to the **whole** block, including statements written before it. That's a Scala 3 scoping rule, and the same one that broke `renderLimited` in 1.1.0. Document it, and put local overrides in an inner block (`locally:`).
- **Constraint found:** the extension can't be `render(indent: Int = 0)`, because it erases to the same signature as the abstract `render(a, indent)`. So the extension is plain `render`, and indentation is internal. A caller who needs a starting indent calls `Renderable[A].render(a, indent)`.

## 4. Decisions

Decided on 2026-10-21. The recommended option was chosen in each case:
- **D1:** the type class is `Renderable`, and the OO trait is removed.
- **D2:** a type with no instance is a compile error; `fromToString` is the explicit opt-in.
- **D3:** Strings are quoted inside structures.
- **D4:** `import Renderable.given` gives `.render` on built-in types.
- **D5:** 2.0 is Scala 3 only, and the rest of the playbook's 2.0 list follows Renderable.

The options as they were proposed:

1. **D1, the name.** The type class takes the name `Renderable` and the OO trait is removed (recommended). The alternative is to keep the trait, for example as `SelfRenderable` with a bridging given, so that classes outside LaScala that extend it keep compiling.
2. **D2, types with no instance.** I recommend a compile error, with `Renderable.fromToString[A]` as the explicit way to opt in. The alternative is a low-priority `toString` fallback, which is the 1.x behaviour but makes it easy to miss an instance without noticing. This also decides how a `Tree[A]` renders values whose type has no instance.
3. **D3, Strings.** Quoted inside structures (recommended, as 1.x does) or always unquoted (as master did).
4. **D4, import for built-in types.** `import Renderable.given` (recommended, as verified) or a top-level `export`, which would make `import com.phasmidsoftware.lascala.*` enough.
5. **D5, the scope of 2.0.** Renderable first, then the rest of the playbook's 2.0 list (recommended): Scala 3 only, givens, enums, indentation syntax, and dropping the Scala 2-only by-name specs. Or Renderable only, staying cross-built.

## 5. Plan once the decisions are made

One commit per step, with the tests passing on 3.3 and 3.9 after each:
1. Build: Scala 3.3.8 only, plus the `"++3.9.0!"` CI step; remove `scala-2.13` and `scala-reflect`.
2. Add `Renderable[A]`, `Layout`, the instances and `derived`, with a new `RenderableSpec` that ports the 1.x expectations.
3. Move `Scalar`, `Node`/`Tree`, `Version`, `Closure` and `RenderableFunction` to givens, and delete the wrappers and `Prefix`.
4. Port the other specs, keeping their expected strings.
5. README: a "Migrating from 1.x" table (playbook §8).
