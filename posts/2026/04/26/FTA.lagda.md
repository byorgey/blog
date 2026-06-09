---
title: 'Proving the Fundamental Theorem of Arithmetic in Agda'
categories: Agda
katex: true
tags: agda,arithmetic,theorem,proof
---

Earlier this spring, I was idly brainstorming potential final projects
for my [Functional Programming](https://hendrix-cs.github.io/csci365/)
students.  Having just [taught my Discrete Math students the
Fundamental Theorem of
Arithmetic](http://ozark.hendrix.edu/~yorgey/forest/00EG/index.xml), I
wondered whether formalizing^[I mean formalizing it *from scratch*: of
course, the FTA is [already in the Agda standard library](https://agda.github.io/agda-stdlib/v2.3/Data.Nat.Primality.Factorisation.html).  As I later
learned, it was added to the standard library by [Nathan van Doorn,
aka Taneb](https://github.com/Taneb).] it in
[Agda](https://wiki.portal.chalmers.se/agda/pmwiki.php?n=Main.HomePage)
could make a nice project.

XXX link to
https://agda.readthedocs.io/en/latest/getting-started/a-taste-of-agda.html
, other Agda documentation?

So I decided to spend about an hour trying to prove the FTA in Agda,
to gauge the level of the project.  At the end of an hour, I had
learned two things: (1) this was in no way an appropriate project for
my students (who have only had a few weeks' practice with Agda); (2) I
was not going to be able to stop until I finished the proof myself.

Over the next week or so, I finished the proof completely from
scratch—without using anything from the standard library, and without
looking up any reference material.  I based it only on my experience
in Agda, knowledge of the relevant proofs on an informal level, and Agda
tricks I've picked up along the way XXX from Conor and elsewhere.

I decided to publish the proof, with extra commentary, in the hopes
that it can be useful as an intermediate-level reference.  That is,
perhaps you've learned some basic Agda and have some basic familiarity
with the Curry-Howard correspondence, but would benefit from seeing an
example of a fully worked out, medium-sized proof.  The resulting blog
post ended up being extremely long, but I didn't think it made sense
to publish it in pieces. XXX?

This blog post XXX available as literate Agda.  XXX There is also an
alternative version of this blog post with holes.  Try filling in the
proofs as you go along, before reading each section.

XXX table of contents.  Maybe skip some parts depending on your background?

## The Fundamental Theorem of Arithmetic

The fundamental theorem of arithmetic states that any natural number
$n \geq 1$ can be written as a product of zero or more primes, and
moreover that this product is unique up to permutation.

For now, we are only going to prove the *existence* part (I may write
another blog post with the uniqueness proof later).  Since a *constructive*
existence proof is really an algorithm for constructing the thing that
is claimed to exist, this can also be seen as a *formally verified
factorization program*: put any number in, get a prime factorization
out.  Writing a prime factorization program is not hard, of course;
it's the formal verification part that is interesting!

## Preliminaries

We will often make use of `A` and `B` to stand for arbitrary
sets/types, so we use a `variable` declaration to tell Agda that it
should implicitly quantify them whenever they show up as free
variables.  That way we don't have to write `{A B : Set} → ...` all the time.

```agda
variable
  A B : Set
```

## Basic logic

Since we're building this completely from scratch, we start with some
types to represent basic logical building blocks (via the
[Curry-Howard correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence)).  First, the "top" type `⊤` to stand for truth, *i.e.* a proposition with trivial evidence:

```agda
data ⊤ : Set where
  tt : ⊤
```

`tt` is declared to be the one and only value of type `⊤`.

Note that some things we define here—such as `⊤`—will have the same
names as they do in the [Agda standard library](XXX).  However, many things
won't, since I either didn't know the standard name and made up my
own, or (in a few cases) did know the standard name but didn't like
it, and made up my own anyway.

Next, the "bottom" type `⊥` with no constructors, representing
falsity, along with a corresponding elimination principle, `absurd`.  The elimination
principle says that anything follows from `⊥` ("*[ex falso
quodlibet](https://en.wikipedia.org/wiki/Principle_of_explosion)*"), and is implemented using Agda's absurd pattern,
written `()`.  If Agda can tell that there are no possible
constructors which could give rise to a value of a certain type, we
can pattern-match on it with `()`, and are absolved of providing a right-hand side for the definition in that case.

```agda
data ⊥ : Set where

absurd : ⊥ → A
absurd ()
```

We can now define negation as an implication to `⊥`.

```agda
¬ : Set → Set
¬ P = P → ⊥
```

Dependent pairs are next: a pair of values where the *type* of the
second component can depend on the *value* of the first.  That is, a value of type `Σ A B` is a value `a` of type
`A` paired with a value of type `B a`.  Via Curry-Howard, this is used
to represent existential quantification: a (constructive) proof of
$\exists a : A.\; B(a)$ is a value $a$ of type $A$ (the *witness*)
paired with a proof that $a$ has property $B$ (*i.e.* a value of type $B(a)$).

```agda
infixr 1 _,_
data Σ (A : Set) (B : A → Set) : Set where
  _,_ : (a : A) → B a → Σ A B
```

We also define a projection function (we only end up needing `fst`;
defining `snd` is left as an exercise for the reader^[The *definition*
of `snd` is trivial; writing down its *type* is a worthwhile
exercise.]), along with a type of non-dependent pairs, corresponding
to logical conjunction (and).

```agda
fst : ∀ {A B} → Σ A B → A
fst (a , _) = a

infixr 3 _×_
_×_ : (A B : Set) → Set
A × B = Σ A (λ _ → B)
```

Finally, we define a disjoint (tagged) union type corresponding to
logical disjunction (or).

```agda
infixr 2 _⊎_
data _⊎_ (A B : Set) : Set where
  inj₁ : A → A ⊎ B
  inj₂ : B → A ⊎ B
```

## Equality

Next, we write down the standard equality (*aka* identity, *aka* path) type, with a single
constructor `refl` that witnesses when its two arguments are
identical.^[It still seems somewhat magical to me that this seemingly
too-simple definition encapsulates everything we want in an equality
relation (well, [almost everything](https://martinescardo.github.io/TypeTopology/UnivalenceFromScratch.html)).]  We also define a convenient
synonym for inequality.

```agda
infix 4 _≡_
data _≡_ (a : A) : A → Set where
  refl : a ≡ a

_≢_ : A → A → Set
x ≢ y = ¬ (x ≡ y)
```

Besides reflexivity, equality enjoys various properties that we will
need: symmetry, transitivity, and congruence (*i.e.*, we can apply any
function to both sides of an equation).

```agda
sym : {x y : A} → x ≡ y → y ≡ x
sym refl = refl

trans : {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl y≡z = y≡z

cong : (f : A → B) → {x y : A} → x ≡ y → f x ≡ f y
cong _ refl = refl
```

Since we will spend a good amount of time reasoning about equality, it
is worthwhile building up some machinery for writing more readable
equality proofs.  Instead of writing, say,
```text
trans p (trans q (trans (sym r) s))
```
we will be able to instead write equality proofs like so:
```text
begin
  v      ≡[ p ⟩≡
  w      ≡[ q ⟩≡
  x      ≡⟨ r ]≡
  y      ≡[ s ⟩≡
  z      ∎
```
This notation is one of my favorite applications of Agda's [mixfix
operator syntax](https://agda.readthedocs.io/en/latest/language/mixfix-operators.html), and has several benefits:

- We can avoid nested parentheses when chaining uses of transitivity.
- We can automatically apply symmetry by using a left-pointing
   instead of right-pointing operator.
- We get to explicitly mention (and have Agda check for us) all the
   intermediate values, making it easier to write the proof
   incrementally, and much easier for humans to read.

This is one of the places where I deliberately chose different
operator names than the standard library, which uses `_≡⟨_⟩_` and
`_≡⟨_⟨_`.  The operator names I decided to use are [inspired by Conor
McBride](https://personal.cis.strath.ac.uk/conor.mcbride/PolyTest.pdf).  I just like the way they look better.

```agda
infix 1 begin_
begin_ : {x y : A} → x ≡ y → x ≡ y
begin x≡y = x≡y

infixr 2 _≡[_⟩≡_
_≡[_⟩≡_ : (x : A) → {y z : A} → (x ≡ y) → (y ≡ z) → (x ≡ z)
_ ≡[ x≡y ⟩≡ y≡z = trans x≡y y≡z

infixr 2 _≡⟨_]≡_
_≡⟨_]≡_ : (x : A) → {y z : A} → (y ≡ x) → (y ≡ z) → (x ≡ z)
_ ≡⟨ y≡x ]≡ y≡z = trans (sym y≡x) y≡z

infixr 5 _∎
_∎ : (x : A) → x ≡ x
_ ∎ = refl
```

Finally, a few [Applicative](XXX)-like operators for more conveniently
writing common forms of congruence.  For example, instead of writing
`cong f x≡y`, we can write `f $≡ x≡y`; or to use congruence on both
arguments of a two-place function at once, we can write `f $≡ x≡y ≡$≡
z≡w`.  (These operators were also inspired by Conor.)

```agda
infixl 4 _$≡_
_$≡_ : (f : A → B) → {x y : A} → x ≡ y → f x ≡ f y
f $≡ x≡y = cong f x≡y

infixl 4 _≡$_
_≡$_ : {f g : A → B} → f ≡ g → (x : A) → f x ≡ g x
f≡g ≡$ x = cong (λ h → h x) f≡g

infixl 4 _≡$≡_
_≡$≡_ : {f g : A → B} → f ≡ g → {x y : A} → x ≡ y → f x ≡ g y
f≡g ≡$≡ x≡y = trans (f≡g ≡$ _) (_ $≡ x≡y)
```

## Natural numbers

Of course, we will need a type to represent the natural numbers.  We
can also tell Agda that our natural number type should correspond to its
built-in notion of natural numbers, so we can use numeric literals
like `2 : ℕ` instead of having to write `suc (suc zero)`.

```agda
data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ
{-# BUILTIN NATURAL ℕ #-}
```

### No confusion

For our natural number type—and often, for any algebraic data type—we
need to know that the constructors are

1. *disjoint*, meaning that different constructors always generate
different values (so it's a contradiction to have an equality
between values built with different constructors); and
2. *injective*, meaning that if we have an equality between values built
with the same constructor, we can decompose it into equalities between
the components.

We can prove both of these simultaneously using a property called "no
confusion".  This is XXX Well-known in the literature.
http://strictlypositive.org/concon.ps.gz
https://link.springer.com/chapter/10.1007/3-540-61780-9_64  Cornes + Terrasse

For natural numbers `m` and `n`, the type `NoConf m n` should be
thought of as the type of evidence that `m ≡ n`, based on looking at
the top-level constructors of `m` and `n`.  If `m` and `n` have different
constructors, then no evidence can possibly show that they are equal,
so `NoConf m n = ⊥` in that case.  If `m` and `n` are both zero, then
they are evidently equal, so `NoConf 0 0 = ⊤`.  Otherwise, if `m` and
`n` are both successors, `NoConf m n` reduces to a proof of equality
between their predecessors.

```agda
NoConf : ℕ → ℕ → Set
NoConf zero zero = ⊤
NoConf zero (suc n) = ⊥
NoConf (suc m) zero = ⊥
NoConf (suc m) (suc n) = m ≡ n
```

Now we can prove the no confusion lemma for our natural number type,
which says that `NoConf m n` always holds whenever `m ≡ n`.
Since `m ≡ n`, we only have to deal with the cases when `m` and `n`
are both zero or both a successor—but this also justifies assigning a
type of `⊥` to the cases when the constructors do not match. `noConf`
can therefore be used to strip `suc` from both sides of an equation,
*or* to derive a contradiction when we have an equation between
non-matching constructors.

```agda
noConf : {m n : ℕ} → m ≡ n → NoConf m n
noConf {zero} refl = tt
noConf {suc m} refl = refl
```

As an aside, this definition of the no confusion property uses a
technique I like: defining a *type* starting with a capital letter,
then defining a *term* that returns that type starting with a
lowercase letter.  This pattern will come up again later.  Sometimes
we define named types in this way just for convenience, say, to be
able to refer to the type multiple times in a concise way; or, as in
the above case, sometimes the type is actually defined via some
nontrivial computation.

### Decidable equality

We can now show how to decide equality of natural numbers.  We first
define a simple type representing decidability in general: `Dec P` represents
either a proof of `P`, or a proof of `¬ P`.^[You may be aware that the
[law of excluded middle](XXX), which
says that $P \lor \neg P$ for all propositions $P$, does not hold in constructive logic.  However, even
though $P \lor \neg P$ does not hold for *all* $P$, it can still hold
for certain specific propositions.  Propositions $P$ for which $P \lor
\neg P$ holds constructively are called *decidable*.]  (The [standard library
version is much more sophisticated](https://agda.github.io/agda-stdlib/v2.3/Relation.Nullary.Decidable.Core.html#1966), but this simple version will do
just fine.)

```agda
data Dec (P : Set) : Set where
  yes : P → Dec P
  no : ¬ P → Dec P
```

We can then prove that for any natural numbers `x` and `y`, we can decide
whether `x ≡ y`.  Notice the several different uses of the no
confusion lemma: two to handle impossible situations, and one to strip
`suc` off both sides of an equality.

```agda
_≟_ : (x y : ℕ) → Dec (x ≡ y)
zero ≟ zero = yes refl
zero ≟ suc y = no noConf
suc x ≟ zero = no noConf
suc x ≟ suc y with x ≟ y
... | yes x≡y = yes (suc $≡ x≡y)
... | no x≢y = no (λ sx≡sy → x≢y (noConf sx≡sy))
```

## Addition

We next turn to defining addition (by pattern-matching on the
left-hand argument), along with several properties of
addition we will need: zero is a right identity for addition; we can
pull out a `suc` from the right-hand argument; and addition is
commutative, associative, and left-cancellable.

```agda
infixl 6 _+_
_+_ : ℕ → ℕ → ℕ
zero + y = y
suc x + y = suc (x + y)

_+0 : (n : ℕ) → (n + 0 ≡ n)
zero +0 = refl
(suc n) +0 = suc $≡ (n +0)

_+suc_ : (x y : ℕ) → (x + suc y) ≡ suc (x + y)
zero +suc y = refl
(suc x) +suc y = suc $≡ (x +suc y)

+-comm : (x y : ℕ) → x + y ≡ y + x
+-comm zero y = sym (y +0)
+-comm (suc x) y = trans (suc $≡ (+-comm x y)) (sym (y +suc x))

+-assoc : (x y z : ℕ) → (x + y) + z ≡ x + (y + z)
+-assoc zero y z = refl
+-assoc (suc x) y z = suc $≡ (+-assoc x y z)

+-cancelˡ : (x y z : ℕ) → x + y ≡ x + z → y ≡ z
+-cancelˡ zero y z x+y≡x+z = x+y≡x+z
+-cancelˡ (suc x) y z x+y≡x+z = +-cancelˡ x y z (noConf x+y≡x+z)
```

## Multiplication

Multiplication is next: we start by defining the multiplication
operation (by pattern-matching on the left-hand argument) and proving
a few lemmas about multiplying by known arguments on the right.

```agda
infixl 7 _*_
_*_ : ℕ → ℕ → ℕ
zero * y = zero
suc x * y = y + x * y

_*0 : (n : ℕ) → (n * 0 ≡ 0)
zero *0 = refl
(suc n) *0 = n *0

_*1 : (n : ℕ) → (n * 1 ≡ n)
0 *1 = refl
(suc n) *1 = suc $≡ (n *1)

_*suc_ : (x y : ℕ) → (x * suc y ≡ x + x * y)
zero *suc y = refl
(suc x) *suc y = suc $≡ (
  begin
  y + x * suc y               ≡[ (y +_) $≡ (x *suc y) ⟩≡
  y + (x + x * y)             ≡⟨ +-assoc y x (x * y) ]≡
  (y + x) + x * y             ≡[ _+_ $≡ +-comm y x ≡$ x * y ⟩≡
  (x + y) + x * y             ≡[ +-assoc x _ _ ⟩≡
  x + (y + x * y)             ∎)
```

We prove some standard properties of multiplication: commutativity,
distributivity over addition, associativity.  The proofs mostly
consist of a whole bunch of algebra, using the special notation for
building chained equality proofs.

```agda
*-comm : (x y : ℕ) → x * y ≡ y * x
*-comm zero y = sym (y *0)
*-comm (suc x) y = begin
  y + x * y                   ≡[ y +_ $≡ *-comm x y ⟩≡
  y + y * x                   ≡⟨ y *suc x ]≡
  y * suc x                   ∎

*-distribˡ : (x y z : ℕ) → x * (y + z) ≡ x * y + x * z
*-distribˡ zero y z = refl
*-distribˡ (suc x) y z = begin
  y + z + x * (y + z)         ≡[ (y + z) +_ $≡ *-distribˡ x y z ⟩≡
  y + z + (x * y + x * z)     ≡[ +-assoc y _ _ ⟩≡
  y + (z + (x * y + x * z))   ≡⟨ y +_ $≡ +-assoc z _ _ ]≡
  y + ((z + x * y) + x * z)   ≡[ y +_ $≡ (_+_ $≡ +-comm z _ ≡$ x * z) ⟩≡
  y + ((x * y + z) + x * z)   ≡[ y +_ $≡ +-assoc (x * y) _ _ ⟩≡
  y + (x * y + (z + x * z))   ≡⟨ +-assoc y _ _ ]≡
  y + x * y + (z + x * z)     ∎

*-distribʳ : (x y z : ℕ) → (x + y) * z ≡ x * z + y * z
*-distribʳ x y z = begin
  (x + y) * z                 ≡[ *-comm (x + y) _ ⟩≡
  z * (x + y)                 ≡[ *-distribˡ z _ _ ⟩≡
  z * x + z * y               ≡[ _+_ $≡ *-comm z _ ≡$≡ *-comm z _ ⟩≡
  x * z + y * z               ∎

*-assoc : (x y z : ℕ) → (x * y) * z ≡ x * (y * z)
*-assoc zero y z = refl
*-assoc (suc x) y z = begin
  (y + x * y) * z             ≡[ *-distribʳ y _ _ ⟩≡
  y * z + (x * y) * z         ≡[ y * z +_ $≡ *-assoc x _ _ ⟩≡
  y * z + x * (y * z)         ∎
```

Finally, we prove that multiplication is left-cancellative.  This
proof is somewhat tricky—in the case that `x`, `y`, and `z` are all
successors, we need to use the induction hypothesis (*i.e.* a
recursive call to `*-cancelˡ`) on `x` and the
predecessors of `y` and `z`, using the fact that + is
left-cancellative to construct the required input equality.

```agda
*-cancelˡ : (x y z : ℕ) → (0 ≢ x) → x * y ≡ x * z → y ≡ z
*-cancelˡ zero y z x≢0 xy≡xz = absurd (x≢0 refl)
*-cancelˡ (suc x) zero zero x≢0 xy≡xz = refl
*-cancelˡ (suc x) zero (suc z) x≢0 xy≡xz = absurd (noConf (trans (sym (x *0)) xy≡xz))
*-cancelˡ (suc x) (suc y) zero x≢0 xy≡xz = absurd (noConf (trans xy≡xz (x *0)))
*-cancelˡ (suc x) (suc y) (suc z) x≢0 xy≡xz = suc $≡
  ( *-cancelˡ (suc x) y z x≢0
    ( +-cancelˡ (suc x) (suc x * y) (suc x * z)
      ( begin
          suc x + suc x * y   ≡⟨ (suc x) *suc y ]≡
          suc x * suc y       ≡[ xy≡xz ⟩≡
          suc x * suc z       ≡[ (suc x) *suc z ⟩≡
          suc x + suc x * z   ∎
      )
    )
  )
```

## Inequality

Next, we give a standard definition of the "less than or equal to"
relation on natural numbers. Note that the structure of a proof of $x \leq
y$ exactly matches the structure of $x$ itself.

```agda
data _≤_ : ℕ → ℕ → Set where
  zle : {n : ℕ} → zero ≤ n
  sle : {m n : ℕ} → m ≤ n → suc m ≤ suc n
```

We also prove some standard properties of $\leq$: it is reflexive and
transitive, and is related to `suc` in various ways.

```agda
≤-refl : {m : ℕ} → m ≤ m
≤-refl {zero} = zle
≤-refl {suc m} = sle ≤-refl

≤-trans : {x y z : ℕ} → x ≤ y → y ≤ z → x ≤ z
≤-trans zle y≤z = zle
≤-trans (sle x≤y) (sle y≤z) = sle (≤-trans x≤y y≤z)

≤-sucr : {m n : ℕ} → m ≤ n → m ≤ suc n
≤-sucr zle = zle
≤-sucr (sle m≤n) = sle (≤-sucr m≤n)

≤-sucl : {m n : ℕ} → suc m ≤ n → m ≤ n
≤-sucl (sle sm≤n) = ≤-sucr sm≤n

≤-pred : {x y : ℕ} → suc x ≤ suc y → x ≤ y
≤-pred (sle sx≤sy) = sx≤sy

```

For convenience, we define $<$ in terms of $\leq$, and prove a few
properties: any number is less than its successor, and $<$ is
transitive and non-reflexive.

```agda
_<_ : ℕ → ℕ → Set
x < y = suc x ≤ y

_<suc : (x : ℕ) → x < suc x
_<suc zero = sle zle
_<suc (suc x) = sle (x <suc)

<-trans : {x y z : ℕ} → x < y → y < z → x < z
<-trans (sle x<y) (sle y<z) = ≤-trans (sle x<y) (≤-sucr y<z)

x≮x : {x : ℕ} → ¬ (x < x)
x≮x {zero} = λ ()
x≮x {suc x} = λ { (sle x<x) → x≮x x<x}
```

### Relationships among equality and inequality

Of course, equality, $<$ and $\leq$ have various relationships that we
will need.  First, equality implies $\leq$.

```agda
≡→≤ : {x y : ℕ} → x ≡ y → x ≤ y
≡→≤ refl = ≤-refl
```

Next, $x < y$ implies that $x$ and $y$ are *not* related by $\equiv$ or
$\leq$.  The first lemma in particular—that $<$ implies $\not\equiv$—gets used quite a bit.  Note that it can be read in two equivalent
ways: on the surface, it is a way to turn a proof of $x < y$ into a
proof of $x \not\equiv y$; but
since $x \not\equiv y$ is really an abbreviation for $(x \equiv y) \to \bot$, it can be
used to derive a contradiction if we have proofs that $x < y$ and
also $x \equiv y$.

```agda
<→≢ : {x y : ℕ} → x < y → x ≢ y
<→≢ x<y refl = x≮x x<y

<→≰ : {x y : ℕ} → x < y → ¬ (y ≤ x)
<→≰ (sle x<y) (sle y≤x) = <→≰ x<y y≤x
```

If $x \leq y$ but they are not equal, then $x < y$.

```agda
≤≢→< : {x y : ℕ} → x ≤ y → x ≢ y → x < y
≤≢→< {y = zero} zle x≢y = absurd (x≢y refl)
≤≢→< {y = suc y} zle x≢y = sle zle
≤≢→< (sle x≤y) x≢y = sle (≤≢→< x≤y (λ m≡n → x≢y (suc $≡ m≡n)))
```

We will need a form of transitivity that says if $x \leq y$ and $y <
z$, then $x < z$.

```agda
≤-<-trans : {x y z : ℕ} → x ≤ y → y < z → x < z
≤-<-trans x≤y (sle y<z) = ≤-trans (sle x≤y) (sle y<z)
```

Finally, a very specific lemma we will need: if a number is not
equal to either 0 or 1, then it must be greater than or equal to 2.

```agda
¬01-is-≥2 : (a : ℕ) → (a ≢ 0) → (a ≢ 1) → (2 ≤ a)
¬01-is-≥2 zero a≢0 a≢1 = absurd (a≢0 refl)
¬01-is-≥2 (suc zero) a≢0 a≢1 = absurd (a≢1 refl)
¬01-is-≥2 (suc (suc a)) a≢0 a≢1 = sle (sle zle)
```

### Arithmetic and inequality

The last lemmas we need relate arithmetic operations and inequality.
First, adding and multiplying cannot make anything smaller (unless we
multiply by zero, of course).

```agda
≤+ : {x y : ℕ} → x ≤ (x + y)
≤+ {zero} = zle
≤+ {suc x} = sle ≤+

≤* : {x y : ℕ} → (x ≢ 0) → y ≤ (x * y)
≤* {zero} x≢0 = absurd (x≢0 refl)
≤* {suc x} x≢0 = ≤+
```

As a result, if we know that one thing is equal to a sum or product of
other things, we can conclude something about their relative sizes.

```agda
+→≤ : {x y z : ℕ} → x + y ≡ z → x ≤ z
+→≤ refl = ≤+

+→< : {x y z : ℕ} → 0 < y → x + y ≡ z → x < z
+→< {x} {suc y} _ x+y≡z = +→≤ (trans (sym (x +suc y)) x+y≡z)

*→≤ : {x y z : ℕ} → (y ≢ 0) → y * x ≡ z → x ≤ z
*→≤ {x} {y} y≢0 refl = ≤* y≢0
```

## Divisibility, primes, and composites

With the preliminaries out of the way, we can finally get on with the
meat of the problem—and we finally get to make use of a dependent pair!  A *constructive* proof that `a` divides
`b` is a specific natural number witness `k`, along with a proof that `k * a ≡ b`.

```agda
_∣_ : ℕ → ℕ → Set
a ∣ b = Σ ℕ (λ k → k * a ≡ b)
```

Proofs of divisibility are unique—that is, for given $a$ and $b$ there
is at most one value of $k$ such that $ka = b$.  We won't need this,
but it follows easily from the fact that multiplication is
cancellative.  More interesting is the fact that divisibility is
*decidable*—that is, for given numbers $a$ and $b$ we can calculate
either a proof that $a \mid b$, or a proof that $\neg (a \mid b)$.
This will play a starring role later on—to factor a number we need to
be able to try potential divisors and find out whether they work—but proving it is not easy!
It will take us several hundred more lines of Agda to get there.

In any case, using this notion of divisibility, we can now define prime and
composite numbers.  A number $n$ is defined to be prime if it is at
least two, and every $2 \leq d < n$ does *not* divide $n$.

```agda
Prime : ℕ → Set
Prime n = (2 ≤ n) × (∀ (d : ℕ) → (2 ≤ d) → (d < n) → ¬ (d ∣ n))
```

One could equivalently define primality by saying that any divisor of
$n$ must be equal to $1$ or $n$; I just decided I liked this
formulation better, especially because it directly matches up with the
way we will test a number for primality later.

A composite number is one that has a nontrivial divisor—that is, a number $d$
such that $2 \leq d < n$ and $d$ divides $n$.^[Note
that we could easily prove that if $n$ is prime then $n$ is not
composite, and likewise if $n$ is composite then it is not prime, but
we won't end up needing these lemmas.]

```agda
Composite : ℕ → Set
Composite n = Σ ℕ (λ d → 2 ≤ d × d < n × d ∣ n)
```

Unlike proofs of divisibility, proofs of `Composite n` are *not*
unique.  For example, we could prove `Composite 12` by showing that
$2$ is a nontrivial divisor of $12$, or by showing that $3$ is.
Although this does not matter from a purely logical point of view, it
matters computationally; in general, we care which specific proof of
`Composite n` we have.

### Nontrivial divisors come in pairs

Before moving on to other things, we will prove a lemma about
composite numbers.  If $n$ is composite, by definition it has a
nontrivial divisor $a$; but this means it must also have a second
nontrivial divisor $b$ such that $ab = n$.  This fact seems almost
trivial to us.  Indeed, it's easy to show that if $n$ has a divisor
$a$, then it must have another divisor $b$ such that $ab = n$.  The
tricky part is showing that if $a$ is a *nontrivial* divisor, then $b$
is *also* nontrivial.  The proof relies on much of the infrastructure
we have built up about natural numbers, multiplication, and
inequality.

First, we define a type representing two factors of a number $n$: a
pair of proofs that $n$ is composite (*i.e.* two nontrivial divisors
of $n$), along with a proof that the product of those divisors is $n$.

```agda
FactorsOf : ℕ → Set
FactorsOf n = Σ (Composite n × Composite n) (λ {(f₁ , f₂) → fst f₁ * fst f₂ ≡ n})
```

Now, we prove that if $n$ is composite, then it has two nontrivial
factors.  We begin by pattern-matching on the proof that $n$ is
composite, which consists of a divisor $a$, evidence that $a$ is
nontrivial (*i.e.* $2 \leq a$ and $a < n$), and a proof that $a$ is a
divisor of $n$, which itself consists of a number $b$ paired with a
proof that $ba = n$.

```agda
factorsOf : (n : ℕ) → Composite n → FactorsOf n
factorsOf n (a , 2≤a , a<n , b , ba≡n) =
```

To construct the proof of `FactorsOf n`, we need two proofs of
`Composite n` along with a proof that the product of the two divisors
is $n$.  We already have a proof that $ba = n$, so we use that, with
$a$ as the second divisor (replicating the corresponding proof of
`Composite n`), and $b$ as the first. Proving that $b$ is a divisor of
$n$ is easy: $a$ is the witness, and proving that $ab = n$ is easy
since we already know $ba = n$ and multiplication is commutative.  The
only thing left is to prove that $b$ is nontrivial, *i.e.* that $2
\leq b$ and $b < n$.

```agda
  ((b , 2≤b , b<n , a , trans (*-comm a b) ba≡n) , (a , 2≤a , a<n , b , ba≡n)) , ba≡n
```

First, we need a lemma that $0 < n$, which follows because $0 < 1 < a
< n$ (remember that a proof of $1 < a$ is actually defined to be the
same thing as a proof of $2 \leq a$).

```agda
 where
  0<n : 0 < n
  0<n = <-trans (sle zle) (<-trans 2≤a a<n)
```

Next, we tackle $2 \leq b$, by showing that $b$ can't possibly be $0$
or $1$ (using our previous lemma that anything not equal to 0 or 1
must be greater than or equal to 2).

```agda
  2≤b : 2 ≤ b
  2≤b = ¬01-is-≥2 b
```

If $b$ were $0$, then $ba = n$ would imply $0 = n$, but we know
$0 < n$, so this is a contradiction.

```agda
    (λ b≡0 → <→≢ 0<n
      (begin
        0                     ≡[ refl ⟩≡
        0 * a                 ≡⟨ _*_ $≡ b≡0 ≡$ a ]≡
        b * a                 ≡[ ba≡n ⟩≡
        n                     ∎
      )
    )
```

If $b$ were $1$, then $ba = n$ would imply $a = n$, but we know
$a < n$, so this is also a contradiction.

```agda
    (λ b≡1 → <→≢ a<n
      (begin
        a                     ≡⟨ a *1 ]≡
        a * 1                 ≡[ *-comm a 1 ⟩≡
        1 * a                 ≡⟨ _*_ $≡ b≡1 ≡$ a ]≡
        b * a                 ≡[ ba≡n ⟩≡
        n                     ∎
      )
    )
```

Finally, we prove $b < n$, by showing $b \leq n$ and $b \neq n$.

```agda
  b<n : b < n
  b<n = ≤≢→<
```

$b \leq n$ since $ba = n$ and $a$ is not zero (if $a$ were zero it
would contradict the fact that $2 \leq a$).

```agda
    (*→≤ (λ a≡0 → <→≢ (<-trans (sle zle) 2≤a) (sym a≡0)) (trans (*-comm a b) ba≡n))
```

$b \neq n$, since $b = n$ together with $ba = n$ would imply $a = 1$
(since multiplication is cancellative), but $2 \leq a$ so it cannot
equal 1.

```agda
    (λ b≡n → <→≢ 2≤a
      (sym
        (*-cancelˡ n a 1 (<→≢ 0<n)
          (begin
            n * a             ≡⟨ _*_ $≡ b≡n ≡$ a ]≡
            b * a             ≡[ ba≡n ⟩≡
            n                 ≡⟨ n *1 ]≡
            n * 1             ∎
          )
        )
      )
    )
```

## Division

Let's start working our way towards proving that divisibility is
decidable.  To check whether $d \mid n$, the usual idea would be to
divide $n$ by $d$ and check whether we get a remainder of zero.  So we
need to formalize this notion of division.

Specifically, when we divide $n$ by $d$, we expect to get a *quotient* $q$
and a *remainder* $r$, such that $r + qd = n$, and $0 \leq r < d$.
The first condition, $r + qd = n$, just defines what we mean by division: $n$ is
$q$ times $d$, plus a remainder of $r$.  The second condition will ensure
that the result is unique.  We wouldn't want to divide $17$ by $2$ and
end up with a quotient of $6$ and a remainder of $5$; the remainder should be as small as possible.

The `DivMod` type simply encodes these requirements.

```agda
data DivMod (n d q r : ℕ) : Set where
  DM : (r + q * d ≡ n) → (r < d) → DivMod n d q r
```

We can prove a few lemmas about `DivMod`.  First, whenever we have
`DivMod n d q r`, then `d` must be positive, since $r < d$ and $r$ is
a natural number.

```agda
divMod→0<d : {n d q r : ℕ} → DivMod n d q r → 0 < d
divMod→0<d (DM _ r<d) = ≤-<-trans zle r<d
```

We can also show that the remainder is zero if and only if $d
\mid n$:

```agda
mod0→divides : (n d : ℕ) {q : ℕ} → DivMod n d q 0 → d ∣ n
mod0→divides n d {q} (DM eq _) = q , eq

divides→mod0 : (n d : ℕ) → (0 < d) → d ∣ n → Σ ℕ (λ q → DivMod n d q 0)
divides→mod0 n d 0<d (q , qd≡n) = q , (DM qd≡n 0<d)
```

We would also like to show that if the remainder when dividing $n$ by
$d$ is *not* zero, then $d$ does *not* divide $n$.  This is *almost*
the contrapositive of `divides→mod0`—which would be trivial to
show—but not quite: I said "the" remainder, but actually we don't yet
know that the quotient and remainder are unique!  Perhaps we could get
a remainder of 0 and some other remainder for the same $n$ and $d$, by
choosing different quotients?

Of course, quotients and remainders *are* unique: that is, if $q_1,
r_1$ and $q_2, r_2$ both satisfy the properties to be the quotient and
remainder of $n$ divided by $d$, then in fact $q_1 = q_2$ and $r_1 =
r_2$.  But how can we prove this?  The usual idea is to look at the
difference $r_1 - r_2 = dq_1 - dq_2$, which is divisible by $d$; but
since $r_1 < d$ and $r_2 < d$, the only way for the difference $r_1 -
r_2$ to be divisible by $d$ is if in fact $r_1 - r_2 = 0$.  From here
we can also derive $q_1 = q_2$ via algebra.

Subtraction, eh?  In order to formalize this, it seems as though we might
need to define the integers... but there is a better way!

## Absolute difference

The previous informal argument mentioned the difference $r_1 - r_2$.
But we could just as easily have talked about $r_2 - r_1$ instead, and
the same argument would work just as well.  This observation shows
that we do not actually care about the (signed) *difference* between
$r_1$ and $r_2$, but only the *distance* between them.  This means we
can just stick to our well-loved natural numbers, and define a commutative
*absolute difference* function which computes the nonnegative distance
between its two arguments, like so:

```agda
∥_-_∥ : ℕ → ℕ → ℕ
∥ zero - y ∥ = y
∥ suc x - zero ∥ = suc x
∥ suc x - suc y ∥ = ∥ x - y ∥
```

Of course, we will need a lot of small lemmas about the properties of
this operation.  We can start by proving that the distance between two
numbers is 0 if and only if they are equal:

```agda
diff0 : (x : ℕ) → 0 ≡ ∥ x - x ∥
diff0 zero = refl
diff0 (suc x) = diff0 x

diff0→≡ : {x y : ℕ} → 0 ≡ ∥ x - y ∥ → x ≡ y
diff0→≡ {zero} {zero} eq = eq
diff0→≡ {suc x} {suc y} eq = suc $≡ diff0→≡ eq
```

Next, the distance between any number and 0 is the number itself, and
the distance function is commutative.
```agda
∥x-0∥≡x : (x : ℕ) → ∥ x - 0 ∥ ≡ x
∥x-0∥≡x zero = refl
∥x-0∥≡x (suc x) = refl

diff-comm : {x y : ℕ} → ∥ x - y ∥ ≡ ∥ y - x ∥
diff-comm {zero} {zero} = refl
diff-comm {zero} {suc y} = refl
diff-comm {suc x} {zero} = refl
diff-comm {suc x} {suc y} = diff-comm {x} {y}
```

A key lemma supporting the argument outlined in the previous section
is that if $x$ and $y$ are both less than $d$, so is their absolute difference.

```agda
diff-< : {x y d : ℕ} → x < d → y < d → ∥ x - y ∥ < d
diff-< {zero} {y} x<d y<d = y<d
diff-< {suc x} {zero} x<d y<d = x<d
diff-< {suc x} {suc y} x<d y<d = diff-< {x} {y} (<-trans (x <suc) x<d) (<-trans (y <suc) y<d)
```

We can also cancel the same thing being added to both sides, or factor out
the same thing being multiplied by both sides.

```agda
diff-cancelˡ : (a b c : ℕ) → ∥ (a + b) - (a + c) ∥ ≡ ∥ b - c ∥
diff-cancelˡ zero b c = refl
diff-cancelˡ (suc a) b c = diff-cancelˡ a b c

diff-distribʳ : (x y d : ℕ) → ∥ x * d - y * d ∥ ≡ ∥ x - y ∥ * d
diff-distribʳ zero y d = refl
diff-distribʳ (suc x) zero d = ∥x-0∥≡x (d + x * d)
diff-distribʳ (suc x) (suc y) d = begin
  ∥ (d + x * d) - (d + y * d) ∥         ≡[ diff-cancelˡ d (x * d) (y * d) ⟩≡
  ∥ x * d - y * d ∥                     ≡[ diff-distribʳ x y d ⟩≡
  ∥ x - y ∥ * d                         ∎
```

Another key lemma is that if $w + x = y + z$, then $\|w - y\| = \|x -
z\|$ (`sub₂` below). Personally, I found this quite tricky to prove.
The best approach I found was to first prove the simpler lemma that
$x + y = z$ implies $x = \| z - y \|$ (`sub₁`), which can then be used
in several places in the proof of `sub₂`.

```agda
sub₁ : {x y z : ℕ} → x + y ≡ z → x ≡ ∥ z - y ∥
sub₁ {zero} {y} {z} refl = diff0 y
sub₁ {suc x} {zero} {suc z} x+y≡z =
  begin
    suc x                     ≡⟨ suc $≡ x +0 ]≡
    suc (x + 0)               ≡[ x+y≡z ⟩≡
    suc z                     ∎
sub₁ {suc x} {suc y} {suc z} x+y≡z =
  sub₁ {suc x} {y} {z}
    (noConf (trans (suc $≡ sym (x +suc y)) x+y≡z))

sub₂ : {w x y z : ℕ} → w + x ≡ y + z → ∥ w - y ∥ ≡ ∥ x - z ∥
sub₂ {zero} {x} {y} {z} w+x≡y+z = sub₁ (sym w+x≡y+z)
sub₂ {suc w} {x} {zero} {z} w+x≡y+z = trans (sub₁ w+x≡y+z) (diff-comm {z})
sub₂ {suc w} {x} {suc y} {z} w+x≡y+z = sub₂ {w} (noConf w+x≡y+z)
```

## Quotient and remainder are unique

We can now return to prove that quotient and remainder are unique.
First, we show that zero is the only multiple of $d$ which is less than $d$.

```agda
∣<→0 : {d x : ℕ} → d ∣ x → x < d → 0 ≡ x
∣<→0 (zero , ad≡x) x<d = ad≡x
∣<→0 (suc a , ad≡x) x<d = absurd (<→≰ x<d (+→≤ ad≡x))
```

And now for the main event: if we have both `DivMod n d q₁ r₁` and
`DivMod n d q₂ r₂`, then in fact the `q`s and `r`s must be the same.

```agda
divModUnique : {n d q₁ r₁ q₂ r₂ : ℕ} → DivMod n d q₁ r₁ → DivMod n d q₂ r₂ → (q₁ ≡ q₂) × (r₁ ≡ r₂)
divModUnique {n} {d} {q₁} {r₁} {q₂} {r₂} dm@(DM r₁+q₁d≡n r₁<d) (DM r₂+q₂d≡n r₂<d) = q₁≡q₂ , r₁≡r₂
 where
```

Since $r_1 + q_1d = n$ and $r_2 + q_2d = n$, by transitivity and
symmetry we have $r_1 + q_1d = r_2 + q_2d$; then by the `sub₂` lemma,
$\|r_1 - r_2\| = \|q_1d - q_2d\|$.

```agda
  rem-diff : ∥ r₁ - r₂ ∥ ≡ ∥ q₁ * d - q₂ * d ∥
  rem-diff = sub₂ {r₁} (trans r₁+q₁d≡n (sym r₂+q₂d≡n))
```
Next, we can show that $d$ divides the absolute difference $\|r_1 -
  r_2\|$, by factoring it out of $\|q_1 d - q_2 d\|$.
```agda
  d∣r₁-r₂ : d ∣ ∥ r₁ - r₂ ∥
  d∣r₁-r₂ = ∥ q₁ - q₂ ∥ ,
    (begin
      ∥ q₁ - q₂ ∥ * d         ≡⟨ diff-distribʳ q₁ q₂ d ]≡
      ∥ q₁ * d - q₂ * d ∥     ≡⟨ rem-diff ]≡
      ∥ r₁ - r₂ ∥             ∎
    )
```

We can then put three lemmas together to conclude $r_1 = r_2$: first,
since $r_1$ and $r_2$ are both less than $d$, so is their absolute
difference; since $d$ also divides the absolute difference, the
absolute difference must be zero; and finally, an absolute difference
of zero means $r_1$ and $r_2$ must be equal.

```agda
  r₁≡r₂ : r₁ ≡ r₂
  r₁≡r₂ = diff0→≡ (∣<→0 d∣r₁-r₂ (diff-< r₁<d r₂<d))
```

From here, proving $q_1 = q_2$ just requires some algebra.

```agda
  dq₁≡dq₂ : d * q₁ ≡ d * q₂
  dq₁≡dq₂ = +-cancelˡ r₁ (d * q₁) (d * q₂)
    (begin
      r₁ + d * q₁             ≡[ r₁ +_ $≡ *-comm d q₁ ⟩≡
      r₁ + q₁ * d             ≡[ r₁+q₁d≡n ⟩≡
      n                       ≡⟨ r₂+q₂d≡n ]≡
      r₂ + q₂ * d             ≡[ _+_ $≡ sym r₁≡r₂ ≡$≡ *-comm q₂ d ⟩≡
      r₁ + d * q₂             ∎
    )

  q₁≡q₂ : q₁ ≡ q₂
  q₁≡q₂ = *-cancelˡ d q₁ q₂ (<→≢ (divMod→0<d dm)) dq₁≡dq₂
```

Finally, we can use uniqueness of quotients and remainders to show the
lemma we wanted about divisibility and remainders: if $n$ divided by
$d$ has some nonzero number as remainder, then $d$ does not divide
$n$.  If $d$ did divide $n$, then we know we would get a remainder of
$0$; but since remainders are unique, we can't have both a zero and
nonzero remainder.

```agda
modS→¬divides : (n d : ℕ) {q r : ℕ} → DivMod n d q (suc r) → ¬ (d ∣ n)
modS→¬divides n d dm d∣n with divides→mod0 n d (divMod→0<d dm) d∣n
... | q₂ , dm₂ with divModUnique dm dm₂
... | q₁≡q₂ , ()
```

## The division algorithm, take 1

So, given natural numbers $n$ and $d$, how do we compute the quotient
and remainder?  We can write down a type `DivAlg` that expresses what
we want: given some $n$ and $d$, `DivAlg n d` represents the result of
the division algorithm, that is, a pair of numbers $(q,r)$ such that
`DivMod n d q r` holds.

```agda
DivAlg : ℕ → ℕ → Set
DivAlg n d = Σ (ℕ × ℕ) (λ { (q , r) → DivMod n d q r })
```

Then we want a function with a type something like `(n d : ℕ) → DivAlg
n d` (actually this type is not quite correct—can you see why?).
How can we write something with this type?

One simple idea, expressed imperatively, is to start with $q = 0$.
Now, as long as $n \geq d$, subtract $d$ from $n$ and add one to
$q$—if we can find $q$ and $r$ such that $r + qd = n - d$, then $r +
(q+1)d = n$.  Eventually, $n$ must land in the range $0 \leq n < d$,
in which case it will be the remainder, and the current value of $q$
will be the quotient.

### Construct the evidence you would like to pattern-match on

That's the idea, but turning this into a verified constructive algorithm will take some
work.  First, let's formalize the idea of testing whether $n$ is less
than $d$, and decreasing it by $d$ if not.  We'd rather not actually
deal with subtraction, so the idea is to generate either a proof that
$n < d$, or another number $n'$ along with a proof that $n' + d = n$.
We encapsulate this in the following type `Cmp`:

```agda
data Cmp (n d : ℕ) : Set where
  LT : n < d → Cmp n d
  GE : (n′ : ℕ) → (n′ + d ≡ n) → Cmp n d
```

`Cmp n d` represents the result of comparing $n$ and $d$, and is
equivalent to having either $n < d$ or $n \geq d$, but expressed in a
form that is more directly useful to us. **Construct the evidence you
would like to pattern-match on!**  That is, in general, evidence for a
proposition $P$ can take many logically equivalent forms, and you
should pick the form that will make your life easiest at the *use
site*, even if it means you have to work harder to *construct* it in
the first place.  You can write standalone lemmas for constructing
your evidence; but pattern-matching it will happen in the middle of
some bigger proof which ought not to be cluttered by calls to
conversion lemmas.

To construct evidence for `Cmp n d`, we write the function
`decreaseBy?` which decides whether we can decrease `n` by `d` or not.
Writing this function is a bit more work than writing something
of type `(n d : ℕ) → (n < d) ⊎ (d ≤ n)`, but our work will pay off later!

```agda
_decreaseBy?_ : (n d : ℕ) → Cmp n d
zero decreaseBy? zero = GE 0 refl
zero decreaseBy? suc d = LT (sle zle)
suc n decreaseBy? zero = GE (suc n) ((suc n) +0)
suc n decreaseBy? suc d with n decreaseBy? d
... | LT n<d = LT (sle n<d)
... | GE n′ n′+d≡n = GE n′ (trans (n′ +suc d) (suc $≡ n′+d≡n))
```

We can also write a helper function `incDivMod` which encodes the
observation from before, that if $r + qd = n-d$, then $r + (q+1)d =
n$.  Of course we don't actually want to use subtraction, so instead
of writing $n-d$, we work in terms of an $n'$ such that $n' + d = n$.
Proving this requires only some straightforward algebra.

```agda
incDivMod : {n′ n d q r : ℕ} → n′ + d ≡ n → DivMod n′ d q r → DivMod n d (suc q) r
incDivMod {n′} {n} {d} {q} {r} n′+d≡n (DM r+qd≡n′ r<d) = DM r+d+qd≡n r<d
 where
  r+d+qd≡n : r + (d + q * d) ≡ n
  r+d+qd≡n = begin
    r + (d + q * d)           ≡⟨ +-assoc r _ _ ]≡
    (r + d) + q * d           ≡[ _+_ $≡ +-comm r _ ≡$ q * d ⟩≡
    (d + r) + q * d           ≡[ +-assoc d _ _ ⟩≡
    d + (r + q * d)           ≡[ d +_ $≡ r+qd≡n′ ⟩≡
    d + n′                    ≡[ +-comm d _ ⟩≡
    n′ + d                    ≡[ n′+d≡n ⟩≡
    n                         ∎
```

Now it seems like we have everything we need to write the division
algorithm as a recursive algorithm: given $n$ and $d$, check whether
$n$ can be decreased by $d$ or not.  If not, we can return $q = 0$ and
$r = n$.  Otherwise, recurse on $n - d$, returning the same remainder
and an incremented quotient from whatever the recursive call returns, using
`incDivMod` to discharge the proof obligation.  It's just a few lines
of code, right?

```agda
module DivModBad where

  {-# NON_TERMINATING #-}
  divAlg : (n d : ℕ) → DivAlg n d
  divAlg n d with n decreaseBy? d
  ... | LT n<d = (0 , n) , (DM (n +0) n<d)
  ... | GE n′ n′+d≡n with divAlg n′ d
  ... | (q , r) , dm = (suc q , r) , (incDivMod n′+d≡n dm)
```

Well, as you can see, it *is* just a few lines of code, but all is not
well: although this function typechecks, Agda can't tell that it is
terminating! (I added the `NON_TERMINATING` pragma so I could include
this bad version of `divMod` in the code without causing an error.)
The problem is that the recursive call to `divMod` is on `n′`,
which is not a subterm of `n`, but instead comes from the call to
`decreaseBy?`.  Agda has no way of knowing whether the result from
some random function call is going to end up being smaller than the
original input.

Now, you and I can both see that this function does indeed terminate,
but we just need a way to convince Agda of this fact... right?

...have you spotted the flaw?  Remember how I mentioned that the type `(n
d : ℕ) → DivAlg n d` is not quite right?  In fact, the above bad
implementation of `divMod` is *not* terminating, and Agda is quite right to
be worried!  In particular, the function recurses infinitely when given an input of $d
= 0$, since it will keep subtracting $0$ from $n$ forever.  This makes sense, of
course: everyone knows you can't divide by zero *because it makes the
universe go into infinite recursion*.

The correct type for `divAlg is `(n d : ℕ) → (0 < d) → DivAlg n d`,
but we're still going to have trouble convincing Agda that our
algorithm is terminating.  In order to do so, we need to take a detour
through *well-founded induction*.

## Well-founded induction

XXX typical "structural" induction --- recursive calls on subterms.
(Agda is a bit more sophisticated than that, but that's the basic
idea.)  There are more exotic forms of induction, but it turns out we
already have everything we need: we can bootstrap

The idea of well-founded induction starts with the general idea of a
*relation*.  A relation on `A` is just a function that takes two
values of type `A` and produces a type, representing evidence that the
two values are related (according to whatever kind of relationship we
have in mind).

```agda
Rel : Set → Set₁
Rel A = A → A → Set
```

We have already seen quite a few relations: equality, less-than,
less-than-or-equal-to, divisibility.

XXX I will use recursive function / induction interchangeably.

Suppose we're writing a recursive function, with some relation $\prec$
in mind, and for a given input $x$ we're only allowed to make
recursive calls on values $y$ such that $y \prec x$.  If $\prec$ is the
"is a structural subterm of" relation, then we get structural
recursion as usual.  But what if $\prec$ is some other relation?  What
needs to be true about $\prec$ for this to make sense?  In particular, how
can we be sure that the function won't get stuck in infinite recursion?

One's first instinct might be to say that $y \prec x$ needs to imply that
$y$ is "smaller than" $x$ somehow.  But what does "smaller than" mean?
And in fact, "smaller than" doesn't always work: for example, if we are
writing a function over the rational numbers, or even just the
integers, the usual "smaller than" relation does *not* guarantee our
function will terminate; it's possible to continue choosing smaller
and smaller rational numbers or integers forever.

The key idea is exactly that this can't happen: it's not possible to
have an infinite chain of values where each is related to the
previous.  That is, there should be no left-infinite chains $\dots
\prec y_3 \prec y_2 \prec y_1 \prec x$.  Then we are guaranteed that if we keep making
recursive calls on values that are related by $\prec$ to the previous
value, we will have to stop eventually: after some finite number of calls
we will hit a value with nothing else related to it.

A relation $\prec$ with this "no left-infinite chains" property is
called *well-founded*. But how do we encode this idea in Agda?

### Accessibility

Instead of thinking negatively (*no* left-infinite
chains), the key is to think positively: *all* chains to the left of
*every* value are finite.  Call a value *accessible* if all chains
leading to it are finite.  Another way to say this is that a value is
accessible if every value related to it is also accessible:

```agda
data Acc (_≺_ : Rel A) : (x : A) → Set where
  acc : {x : A} → ((y : A) → y ≺ x → Acc _≺_ y) → Acc _≺_ x
```

`Acc _≺_ x` defines what it means for a particular value `x` to be
accessible with respect to a relation `R`.  There is only one
constructor, `acc`, which requires `(y : A) → y ≺ x → Acc _≺_ y`—that
is, for every value `y` of type `A`, if `y` is related to `x`, then
`y` is accessible.  In other words, `x` is accessible if every `y ≺ x`
is accessible.

XXX This is definitely tricky to wrap your head around.  At this point
you may have two objections:

1. What about base cases?  Shouldn't we have another constructor which
   says `x` is accessible if *nothing* is related to it?  Actually,
   the `acc` constructor already says that!  If nothing is related to
   `x`, then `(y : A) → y ≺ x → Acc _≺_ y` is trivially true: we can
   easily promise anything we want as the output of a function if we
   know it can never be called.  Every number less than zero is a
   purple flying weasel.

2. Doesn't this just run into the same problem as before with
   left-infinite chains?  If we consider the "is one less than"
   relation on the integers, isn't $2$ accessible because $1$ is
   accessible because $0$ is accessible because $-1$ is accessible
   because ... ?

   There is something a bit subtle going on here: recursive data types
   in Agda (unlike, say, Haskell) are interpreted according to a
   *least fixed point* semantics.  Put in plain terms, the only values
   of a data type are those which can be built by applications of a
   *finite* number of constructors.  So in fact, the "no left-infinite
   chain" condition is foundationally built into the way Agda data
   types work!

   XXX a bit exotic since `Acc` values can be *infinitely-branching* trees!

XXX turn well-founded induction into structural induction by
*pattern-matching on `Acc` proofs*!

### Well-founded induction

Given the definition of accessible elements, we can now give the
definition of a well-founded relation: a relation on `A` is well-founded if
*every* value of type `A` is accessible.

```agda
WellFounded : Rel A → Set
WellFounded {A} _≺_ = (a : A) → Acc _≺_ a
```

We can now write down the *principle of well-founded induction*.  XXX
also tricky to wrap your brain around.  Before, we were just talking
about functions terminating or not.  But the reason this is important
is that a function might be calculating a *proof*.  A function which
purports to calculate a proof but sometimes goes into infinite
recursion is a charlatan: XXX not really a proof at all.

So instead of thinking about termination, let's switch to thinking
about proofs.  Given a proposition $P(x)$, we want to prove that
$P(x)$ holds for all $x$ of type $A$.  The idea is that when trying to
prove $P(y)$ for a particular $y$, we get to assume that $P(z)$ holds
(*i.e.* we get to make recursive calls $P(z)$) for all $z \prec y$.

XXX

* `P : A → Set`  : the proposition.
* `_≺_ : Rel A`: the relation.
* `WellFounded _≺_`: a proof that `≺` is a well-founded relation.
* `(y : A) → ((z : A) → z ≺ y → P z) → P y`: XXX tricky.  "For any
  `y`, if we know `P z` holds for all `z ≺ y`, then we can show `P y`
  also holds."

The principle of well-founded induction says that this is enough to
show `P x` holds for *all* `x`.

```agda
wf-ind : {P : A → Set} {_≺_ : Rel A} → WellFounded _≺_ → ((y : A) → ((z : A) → z ≺ y → P z) → P y) → (x : A) → P x
```

XXX now, how to implement this?  If we try something
straightforward—just call `ind` on `x`, then call `wf-ind` recursively
to fill in the proof for `P z`—of course it does not work; Agda cannot
tell that this is terminating.  And this makes sense, because we are
not even using the fact that the relation is well-founded at all!

```agda
module WFIndBad where

  -- XXX not sure why this still yields a nontermination error even with the pragma
  {-# NON_TERMINATING #-}
  wf-ind-bad : {P : A → Set} {R : Rel A} → WellFounded R → ((y : A) → ((z : A) → R z y → P z) → P y) → (x : A) → P x
  wf-ind-bad {A} {P} {R} wf ind x = ind x (λ z Rzx → wf-ind-bad wf ind z)
```

The idea is to use the fact that $\prec$ is well-founded to generate
an initial proof of accessibility for the input $x$, and then *pattern
match on accessibility proofs* alongside the values as we recurse.
Every time we make a recursive call on some $y \prec x$, we can just
pattern-match on the accessibility proof for $x$ to get an
accessibility proof for $y$, so Agda will be able to see that the
whole thing is *structurally* recursive on the accessibility proofs.

```agda
wf-ind {A} {P} {R} wf ind x = go x (wf x)
```

```agda
 where
  go : (x : A) → Acc R x → P x
  go x (acc f) = ind x (λ z Rzx → go z (f z Rzx))
```

Now that we have well-founded induction under our belts, let's show
that a couple different relations we want to use are well-founded.
First up is the less-than relation on natural numbers.  The fact that
$<$ on natural numbers is well-founded corresponds to what is often
called "strong induction".^[Incidentally, we could probably have
gotten away with directly defining a principle of strong natural
number induction, without bothering with the full generality of
well-founded induction, but this way is more fun and interesting!] To
prove that $<$ is well-founded, we of course must show that every
natural number is accessible under $<$.  However, if we directly try
to prove `(m : ℕ) → (Acc _<_) m`, we run into a variant of the exact
same problem we have been dealing with: to prove that
$m$ is accessible we need to know that *every* $k < m$ is also
accessible, but again, XXX

What we need is the usual trick for proving strong induction from weak
induction: instead of proving that $P(x)$ holds for all $x$, we prove
that $(\downarrow P)(x)$ holds for all $x$, where $\downarrow P$ is
the "downward closure" of $P$: $(\downarrow P)(x)$ says that $P(x)$
holds for *all* $y \leq x$.

XXX note that this is just the reflexive, transitive closure of the
successor relation.  Advanced exercise: generalize $\downarrow P$ as the
reflexive, transitive closure of $P$ and then prove that $\downarrow
P$ is well-founded whenever $P$ is. In fact, it's an if and only if. (See other blog post on
well-founded induction: https://boarders.github.io/posts/well_founded_induction.html)

```agda
↓ : (ℕ → Set) → (ℕ → Set)
↓ P n = (k : ℕ) → (k ≤ n) → P k
```

Now we can prove, for all natural numbers $m$, that every natural
number up to and including $m$ is accessible under the $<$ relation.  Zero is accessible
because nothing is less than it; everything up to the successor of $m$ is accessible because by induction we know everything up to $m$ is, and anything less than the successor of $m$ must in fact be $\leq m$.

```agda
<-acc : (m : ℕ) → ↓ (Acc _<_) m
<-acc zero b zle = acc (λ a ())
<-acc (suc m) b b≤sm = acc (λ a a<b → <-acc m a (≤-pred (≤-trans a<b b≤sm)))
```

Finally, to show that any natural number $n$ is accessible—*i.e.* that $<$
is well-founded—we can use the fact that all numbers up to and including $n$
are accessible, and just project out accessibility for $n$ itself.

```agda
<-wf : WellFounded _<_
<-wf n = <-acc n n ≤-refl
```

## The division algorithm

Finally, we can define the division algorithm, via well-founded
induction!  The definition is very similar to our first attempt, but we use the principle of well-founded induction with $<$.  Note that neither `divAlg` nor its helper function `go` is directly recursive. Instead, `go` takes an induction hypothesis as an argument, which we call instead, providing an extra proof that the subject of the induction hypothesis is in fact less than the original input.  `wf-ind` takes care of the actual recursion.

```agda
divAlg : (n d : ℕ) → (0 < d) → DivAlg n d
divAlg n d 0<d = wf-ind {P = λ n → DivAlg n d} <-wf go n
 where
  go : (n : ℕ) → ((n′ : ℕ) → n′ < n → DivAlg n′ d) → DivAlg n d
  go n IH with n decreaseBy? d
  ... | LT n<d = (0 , n) , DM (n +0) n<d
  ... | GE n′ n′+d≡n with IH n′ (+→< 0<d n′+d≡n)
  ... | (q , r) , dm = (suc q , r) , incDivMod n′+d≡n dm
```

Using the division algorithm, we can also finally decide whether one
number divides another: zero divides zero; zero does not divide any
successor since that would imply there is some $k$ such that $k$ times
zero is nonzero, which is absurd; and if $x$ is a successor, we can
apply the division algorithm and check the remainder, applying some
previous lemmas that tell us what zero and nonzero remainders tells us
about divisibility.

```agda
_∣?_ : (x y : ℕ) → Dec (x ∣ y)
zero ∣? zero = yes (0 , refl)
zero ∣? (suc y) = no λ { (a , eq) → absurd (noConf (trans (sym (*-comm a zero)) eq))}
(suc x) ∣? y with divAlg y (suc x) (sle zle)
... | (q , zero) , dm = yes (mod0→divides y (suc x) dm)
... | (q , suc r) , dm = no (modS→¬divides y (suc x) dm)
```

## Primality testing

XXX motivate need for loops?

```agda
-- Alternate definition of inequality

data _≤′_ : ℕ → ℕ → Set where
  lerefl : {n : ℕ} → n ≤′ n
  lesuc : {m n : ℕ} → suc m ≤′ n → m ≤′ n

≤′-suc : {m n : ℕ} → m ≤′ n → suc m ≤′ suc n
≤′-suc lerefl = lerefl
≤′-suc (lesuc m≤′n) = lesuc (≤′-suc m≤′n)

0≤′ : (n : ℕ) → 0 ≤′ n
0≤′ zero = lerefl
0≤′ (suc n) = lesuc (≤′-suc (0≤′ n))

≤→≤′ : {m n : ℕ} → m ≤ n → m ≤′ n
≤→≤′ {n = n} zle = 0≤′ n
≤→≤′ (sle m≤n) = ≤′-suc (≤→≤′ m≤n)

-- Loops

data _∈[_⋯_] : ℕ → ℕ → ℕ → Set where
  stop : {a b : ℕ} → a ≤ b → b ∈[ a ⋯ b ]
  step : {a i b : ℕ} → a ≤ i → suc i ∈[ a ⋯ b ] → i ∈[ a ⋯ b ]

loop : (a b : ℕ) → (a ≤′ b) → a ∈[ a ⋯ b ]
loop a b a≤′b = mid a a b ≤-refl a≤′b
 where
  mid : (a i b : ℕ) → (a ≤ i) → (i ≤′ b) → i ∈[ a ⋯ b ]
  mid a i b a≤i lerefl = stop a≤i
  mid a i b a≤i (lesuc i≤′b) = step a≤i (mid a (suc i) b (≤-sucr a≤i) i≤′b)

top : {i a b : ℕ} → i ∈[ a ⋯ b ] → i ≤ b
top (stop _) = ≤-refl
top (step _ s) = ≤-sucl (top s)

------------------------------------------------------------
-- Primality testing
------------------------------------------------------------

add : {P : ℕ → Set} {m : ℕ} → ((j : ℕ) → (2 ≤ j) → (j < m) → P j) → P m → ((j : ℕ) → (2 ≤ j) → (j < suc m) → P j)
add {m = m} f Pm j le lt with (j ≟ m)
... | yes refl = Pm
... | no j≢m = f j le (≤≢→< (≤-pred lt) j≢m)

prime? : (n : ℕ) → (2 ≤ n) → Prime n ⊎ Composite n
prime? n 2≤n = try (loop 2 n (≤→≤′ 2≤n)) (λ { (suc zero) (sle ()) j<2 _ ; (suc (suc j)) 2≤j (sle (sle ())) _})
 where
  try : {m : ℕ} → m ∈[ 2 ⋯ n ] → ((j : ℕ) → (2 ≤ j) → (j < m) → ¬ (j ∣ n)) → Prime n ⊎ Composite n
  try (stop 2≤n) soFar = inj₁ (2≤n , soFar)
  try {m} (step 2≤m next) soFar with m ∣? n
  ... | yes m∣n = inj₂ (m , 2≤m , top next , m∣n)
  ... | no m∤n = try next (add soFar m∤n)
```

## Lists

Before we are able to state the Fundamental Theorem of Arithmetic, we need to build up a data type for lists, along with some standard list manipulation functions.  First, we define the type of lists and the standard `foldr` function.

```agda
data List (A : Set) : Set where
  [] : List A
  _∷_ : A → List A → List A

foldr : (A → B → B) → B → List A → B
foldr _&_ z [] = z
foldr _&_ z (x ∷ xs) = x & foldr _&_ z xs
```

Now we can define concatenation and product via `foldr`.

```agda
_++_ : List A → List A → List A
xs ++ ys = foldr (_∷_) ys xs

product : List ℕ → ℕ
product = foldr _*_ 1
```

We will need `All`, which expresses that some predicate holds of all the elements of a list.  In fact, `All` is manifestly an instance of `foldr`, but we would need a universe-polymorphic version of `foldr` for that, so we just write it manually.

```agda
All : (P : A → Set) → List A → Set
All P [] = ⊤
All P (x ∷ xs) = P x × All P xs
```

Now, we just need a couple lemmas about concatenation: first, that if `P` holds for all the elements in `xs` and all the elements in `ys`, then it holds for all the elements in `xs ++ ys`; and second, that `product` distributes over concatenation (*i.e.* it is a homomorphism from the monoid of lists under concatenation to the monoid of natural numbers under multiplication).

```agda
All-++ : {P : A → Set} {xs ys : List A} → All P xs → All P ys → All P (xs ++ ys)
All-++ {xs = []} Pxs Pys = Pys
All-++ {xs = x ∷ xs} (Px , Pxs) Pys = Px , All-++ Pxs Pys

product-++ : (xs ys : List ℕ) → product (xs ++ ys) ≡ product xs * product ys
product-++ [] ys = sym (_ +0)
product-++ (x ∷ xs) ys = trans (x *_ $≡ product-++ xs ys) (sym (*-assoc x (product xs) (product ys)))
```

## The Fundamental Theorem of Arithmetic

Finally, we can put all the pieces together to state and prove (one
half of) the Fundamental Theorem of Arithmetic!  `FTA n` says that for
some positive integer `n`, we can find a list of natural numbers which
are all prime, and whose product is `n`.

```agda
FTA : ℕ → Set
FTA n = Σ (List ℕ) (λ ps → All Prime ps × product ps ≡ n)
```

To prove that this holds for all positive integers, we can again use
well-founded induction: in the base case, if $n = 1$, the empty list
suffices.  Otherwise, we can decide whether $n$ is prime. If so, the
singleton list containing $n$ fits the bill. Otherwise, $n = ab$ where
$a$ and $b$ are both nontrivial divisors of $n$; by the induction
hypothesis, both can be factored into primes, and the list we want for
$n$ is simply the concatenation of the factorizations for $a$ and $b$.

```agda
fta : (n : ℕ) → (0 < n) → FTA n
fta n = wf-ind {P = (λ n → (0 < n) → FTA n)} <-wf go n
 where
  go : (n : ℕ) → ((n′ : ℕ) → n′ < n → 0 < n′ → FTA n′) → 0 < n → FTA n
  go (suc zero) IH 0<n = [] , (tt , refl)
  go (suc (suc n)) IH 0<n with prime? (suc (suc n)) (sle (sle zle))
  ... | inj₁ P = (suc (suc n) ∷ []) , ((P , tt) , (suc $≡ (suc $≡ (n *1))))
  ... | inj₂ C with factorsOf _ C
  ... | ((a , sle _ , a<n , _) , (b , sle _ , b<n , _)) , ab≡n
      with IH a a<n (sle zle)
         | IH b b<n (sle zle)
  ... | ps₁ , Pps₁ , prod₁ | ps₂ , Pps₂ , prod₂ = (ps₁ ++ ps₂) , (All-++ Pps₁ Pps₂) ,
    begin
      product (ps₁ ++ ps₂)      ≡[ product-++ ps₁ ps₂ ⟩≡
      product ps₁ * product ps₂ ≡[ _*_ $≡ prod₁ ≡$≡ prod₂ ⟩≡
      a * b                     ≡[ ab≡n ⟩≡
      suc (suc n)               ∎
```

XXX conclusion?
