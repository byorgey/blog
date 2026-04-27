---
title: 'Proving the Fundamental Theorem of Arithmetic in Agda'
categories: Agda
katex: true
tags: agda,arithmetic,theorem,proof
---

A couple weeks ago, I was idly brainstorming potential final projects
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
example of a fully worked out, medium-sized proof.

This blog post XXX available as literate Agda.  XXX There is also an
alternative version of this blog post with holes.  Try filling in the
proofs as you go along, before reading each section.

## The Fundamental Theorem of Arithmetic

The fundamental theorem of arithmetic states that any natural number
$n \geq 1$ can be written as a product of zero or more primes, and
moreover that this product is unique up to permutation.

For now, we are only going to prove *existence* part (I may write
another blog post with the uniqueness proof later).  Since a *constructive*
existence proof is really an algorithm for constructing the thing that
is claimed to exist, this can also be seen as a *formally verified
factorization program*: put any number in, get a prime factorization
out.  Writing a prime factorization program is not hard, of course;
it's the formal verification part that is particularly interesting!

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
[Curry-Howard correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence)).  First, the "top" type `⊤` to
stand for truth, *i.e.* a proposition with trivial evidence:

```agda
data ⊤ : Set where
  tt : ⊤
```

Note that some things we define here—such as `⊤`—will have the same
names as they do in the Agda standard library.  However, many things
won't, since I either didn't know the standard name and made up my
own, or (in a few cases) did know the standard name but didn't like
it, and made up my own anyway.

Next, the "bottom" type `⊥` with no constructors, representing
falsity, along with a corresponding elimination principle, `absurd`.  The elimination
principle says that anything follows from `⊥` ("*[ex falso
quodlibet](https://en.wikipedia.org/wiki/Principle_of_explosion)*"), and is implemented using Agda's absurd pattern,
written `()`.  If Agda can tell that there are no possible
constructors which could give rise to a value of a certain type, we
can pattern-match on it with `()`, and are then absolved of having to
provide a right-hand side for the definition in that case.

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

The standard equality (*aka* identity *aka* path) type, with a single
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

1. We can avoid nested parentheses when chaining uses of transitivity.
2. We can automatically apply symmetry by using a left-pointing
   instead of right-pointing operator.
3. We get to explicitly mention (and have Agda check for us) all the
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
can tell Agda that our natural number type should correspond to its
built-in notion of natural numbers, so we can use numeric literals
like `2 : ℕ` instead of having to write `suc (suc zero)`.

```agda
data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ
{-# BUILTIN NATURAL ℕ #-}
```

XXX for any data type, often need to know that XXX at least in a
vanilla data type with no quotients, constructors are
(1) disjoint, meaning that different constructors always generate
different values (aka it's a contradiction to have  an equality
between values built with different constructors); (2) injective, i.e. if we have an equality between values built
with the same constructor, we can decompose it into equalities between
the components.   XXX see http://strictlypositive.org/concon.ps.gz
XXX can prove both simultaneously using a property called "no
confusion".  XXX Well-known in the literature.

There is a
XXX In general I like the pattern of defining a *type* starting with a
capital letter, then a *term* that returns that type starting with a
lowercase letter.  We will use this pattern later too (DivAlg/divAlg, FTA/fta).  Sometimes just
for convenience.  In this case, the type is actually defined via some
nontrivial computation.

XXX For natural numbers `m` and `n`, the type `NoConf m n` should be
thought of as the type of evidence that `m ≡ n`, based on looking at
one level of constructor XXX.  If `m` and `n` have different
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

We now show how to decide equality of natural numbers.  We can first
define a simple type representing decidability in general: `Dec P` represents
either a proof of `P`, or a proof of `¬ P`.  (The [standard library
version is much more sophisticated](https://agda.github.io/agda-stdlib/v2.3/Relation.Nullary.Decidable.Core.html#1966), but this simple version will do
just fine.)

```agda
data Dec (P : Set) : Set where
  yes : P → Dec P
  no : ¬ P → Dec P
```

We can then prove that for any natural numbers `x` and `y`, we can decide
whether `x ≡ y`.  Notice the several different uses of the no
confusion lemma.

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

Multiplication is next:

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

--------------------------------------------------
-- Inequality

-- ≤

data _≤_ : ℕ → ℕ → Set where
  zle : {n : ℕ} → zero ≤ n
  sle : {m n : ℕ} → m ≤ n → suc m ≤ suc n

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

-- <

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

-- Relationships among ≤, <, ≡

≡→≤ : {x y : ℕ} → x ≡ y → x ≤ y
≡→≤ refl = ≤-refl

<→≢ : {x y : ℕ} → x < y → x ≢ y   -- gets used a lot!
<→≢ x<y refl = x≮x x<y

<→≰ : {x y : ℕ} → x < y → ¬ (y ≤ x)
<→≰ (sle x<y) (sle y≤x) = <→≰ x<y y≤x

≤≢→< : {x y : ℕ} → x ≤ y → x ≢ y → x < y
≤≢→< {y = zero} zle x≢y = absurd (x≢y refl)
≤≢→< {y = suc y} zle x≢y = sle zle
≤≢→< (sle x≤y) x≢y = sle (≤≢→< x≤y (λ m≡n → x≢y (suc $≡ m≡n)))

≤-<-trans : {x y z : ℕ} → x ≤ y → y < z → x < z
≤-<-trans x≤y (sle y<z) = ≤-trans (sle x≤y) (sle y<z)

¬01-is-≥2 : (a : ℕ) → (a ≢ 0) → (a ≢ 1) → (2 ≤ a)
¬01-is-≥2 zero a≢0 a≢1 = absurd (a≢0 refl)
¬01-is-≥2 (suc zero) a≢0 a≢1 = absurd (a≢1 refl)
¬01-is-≥2 (suc (suc a)) a≢0 a≢1 = sle (sle zle)

-- Arithmetic and inequality

+→≤ : {x y z : ℕ} → x + y ≡ z → x ≤ z
+→≤ {zero} x+y≡z = zle
+→≤ {suc x} {z = suc z} x+y≡z = sle (+→≤ (noConf x+y≡z))

+→< : {x y z : ℕ} → 0 < y → x + y ≡ z → x < z
+→< {x} {suc y} _ x+y≡z = +→≤ (trans (sym (x +suc y)) x+y≡z)

≤-+ : {x y : ℕ} → x ≤ (x + y)
≤-+ {zero} = zle
≤-+ {suc x} = sle ≤-+

≤-* : {x y : ℕ} → (x ≢ 0) → y ≤ (x * y)
≤-* {zero} x≢0 = absurd (x≢0 refl)
≤-* {suc x} x≢0 = ≤-+

*→≤ : {x y z : ℕ} → (y ≢ 0) → x * y ≡ z → x ≤ z
*→≤ {x} {y} y≢0 refl = ≤-trans (≤-* y≢0) (≡→≤ (*-comm y x))

--------------------------------------------------
-- Divisibility, primes, and composites

_∣_ : ℕ → ℕ → Set
a ∣ b = Σ ℕ (λ k → k * a ≡ b)

Prime : ℕ → Set
Prime n = (2 ≤ n) × (∀ (d : ℕ) → (2 ≤ d) → (d < n) → ¬ (d ∣ n))

Composite : ℕ → Set
Composite n = Σ ℕ (λ d → 2 ≤ d × d < n × d ∣ n)

FactorsOf : ℕ → Set
FactorsOf n = Σ (Composite n × Composite n) (λ {(f₁ , f₂) → fst f₁ * fst f₂ ≡ n})

factors : (n : ℕ) → Composite n → FactorsOf n
factors n (a , 2≤a , a<n , b , ba≡n) =
  ((b , 2≤b , b<n , a , trans (*-comm a b) ba≡n) , (a , 2≤a , a<n , b , ba≡n)) , ba≡n
 where
  0<n : 0 < n
  0<n = <-trans (sle zle) (<-trans 2≤a a<n)

  -- 2≤b because...
  2≤b : 2 ≤ b
  2≤b = ¬01-is-≥2 b
    -- if b was 0, then ba=n implies n=0, but 2≤a<n
    (λ b≡0 → <→≢ 0<n (begin 0 ≡[ refl ⟩≡ 0 * a ≡⟨ _*_ $≡ b≡0 ≡$ a ]≡ b * a ≡[ ba≡n ⟩≡ n ∎))
    -- if b was 1, then a=n, but a<n
    (λ b≡1 → <→≢ a<n
      (begin
        a                     ≡⟨ a *1 ]≡
        a * 1                 ≡[ *-comm a 1 ⟩≡
        1 * a                 ≡⟨ _*_ $≡ b≡1 ≡$ a ]≡
        b * a                 ≡[ ba≡n ⟩≡
        n                     ∎
      )
    )

  -- b<n because...
  b<n : b < n
  b<n = ≤≢→<
    -- ba=n implies b≤n
    (*→≤ (λ a≡0 → <→≢ (<-trans (sle zle) 2≤a) (sym a≡0)) ba≡n)
    -- if b=n then a=1 but 2≤a.
    (λ b≡n → <→≢ 2≤a (sym (*-cancelˡ n a 1 (<→≢ 0<n) (begin n * a ≡⟨ _*_ $≡ b≡n ≡$ a ]≡ b * a ≡[ ba≡n ⟩≡ n ≡⟨ n *1 ]≡ n * 1 ∎))))

------------------------------------------------------------
-- Abs diff
------------------------------------------------------------

∥_-_∥ : ℕ → ℕ → ℕ
∥ zero - y ∥ = y
∥ suc x - zero ∥ = suc x
∥ suc x - suc y ∥ = ∥ x - y ∥

diff0 : (x : ℕ) → 0 ≡ ∥ x - x ∥
diff0 zero = refl
diff0 (suc x) = diff0 x

diff0→≡ : {x y : ℕ} → 0 ≡ ∥ x - y ∥ → x ≡ y
diff0→≡ {zero} {zero} eq = eq
diff0→≡ {suc x} {suc y} eq = suc $≡ diff0→≡ eq

∥x-0∥≡x : (x : ℕ) → ∥ x - 0 ∥ ≡ x
∥x-0∥≡x zero = refl
∥x-0∥≡x (suc x) = refl

diff-comm : {x y : ℕ} → ∥ x - y ∥ ≡ ∥ y - x ∥
diff-comm {zero} {zero} = refl
diff-comm {zero} {suc y} = refl
diff-comm {suc x} {zero} = refl
diff-comm {suc x} {suc y} = diff-comm {x} {y}

∣<→0 : {d x : ℕ} → d ∣ x → x < d → 0 ≡ x
∣<→0 (zero , ad≡x) x<d = ad≡x
∣<→0 (suc a , ad≡x) x<d = absurd (<→≰ x<d (+→≤ ad≡x))

diff-< : {x y d : ℕ} → x < d → y < d → ∥ x - y ∥ < d
diff-< {zero} {y} x<d y<d = y<d
diff-< {suc x} {zero} x<d y<d = x<d
diff-< {suc x} {suc y} x<d y<d = diff-< {x} {y} (<-trans (x <suc) x<d) (<-trans (y <suc) y<d)

diff-cancelˡ : (a b c : ℕ) → ∥ (a + b) - (a + c) ∥ ≡ ∥ b - c ∥
diff-cancelˡ zero b c = refl
diff-cancelˡ (suc a) b c = diff-cancelˡ a b c

diff-distribʳ : (x y d : ℕ) → ∥ x * d - y * d ∥ ≡ ∥ x - y ∥ * d
diff-distribʳ zero y d = refl
diff-distribʳ (suc x) zero d = ∥x-0∥≡x (d + x * d)
diff-distribʳ (suc x) (suc y) d = begin
  ∥ (d + x * d) - (d + y * d) ∥
    ≡[ diff-cancelˡ d (x * d) (y * d) ⟩≡
  ∥ x * d - y * d ∥
    ≡[ diff-distribʳ x y d ⟩≡
  ∥ x - y ∥ * d
  ∎

sub : {x y z : ℕ} → x + y ≡ z → x ≡ ∥ z - y ∥
sub {zero} {y} {z} refl = diff0 y
sub {suc x} {zero} {suc z} x+y≡z = begin suc x ≡⟨ suc $≡ x +0 ]≡ suc (x + 0) ≡[ x+y≡z ⟩≡ suc z ∎
sub {suc x} {suc y} {suc z} x+y≡z = sub {suc x} {y} {z} (noConf (trans (suc $≡ sym (x +suc y)) x+y≡z))

sub₂ : {w x y z : ℕ} → w + x ≡ y + z → ∥ w - y ∥ ≡ ∥ x - z ∥
sub₂ {zero} {x} {y} {z} w+x≡y+z = sub (sym w+x≡y+z)
sub₂ {suc w} {x} {zero} {z} w+x≡y+z = trans (sub w+x≡y+z) (diff-comm {z})
sub₂ {suc w} {x} {suc y} {z} w+x≡y+z = sub₂ {w} (noConf w+x≡y+z)

------------------------------------------------------------
-- Well-founded induction
------------------------------------------------------------

Rel : Set → Set₁
Rel A = A → A → Set

data Acc (R : Rel A) : (a : A) → Set where
  -- b is accessible if all elements below it are accessible
  acc : {b : A} → ((a : A) → R a b → Acc R a) → Acc R b

-- Relation is well-founded if all elements are accessible
WellFounded : Rel A → Set
WellFounded {A} R = (a : A) → Acc R a

-- Well-founded induction.
wf-ind : {P : A → Set} {R : Rel A} → WellFounded R → ((y : A) → ((z : A) → R z y → P z) → P y) → (x : A) → P x
wf-ind {A} {P} {R} wf ind x = go x (wf x)
 where
  go : (x : A) → Acc R x → P x
  go x (acc f) = ind x (λ z Rzx → go z (f z Rzx))

--------------------------------------------------
-- _<_ is well-founded

-- ↓ P is the downward closure of P, i.e. ↓ P n is the proposition that P holds for all k ≤ n.
↓ : (ℕ → Set) → (ℕ → Set)
↓ P n = (k : ℕ) → (k ≤ n) → P k

-- Prove ↓ (Acc _<_)  for all n  by induction, using transitivity of ≤ .
<-acc : (m : ℕ) → ↓ (Acc _<_) m
<-acc zero b zle = acc (λ a ())
<-acc (suc m) b b≤sm = acc (λ a a<b → <-acc m a (≤-pred (≤-trans a<b b≤sm)))

-- To show any n is accessible, use the fact that ↓ (Acc _<_) n is true and just project out accessibility for n.
<-wf : WellFounded _<_
<-wf n = <-acc n n ≤-refl

--------------------------------------------------
-- ℤ⁺, <⁺ is well-founded

ℤ⁺ : Set
ℤ⁺ = Σ ℕ (λ n → 0 < n)

_<⁺_ : ℤ⁺ → ℤ⁺ → Set
(a , _) <⁺ (b , _) = a < b

acc<→acc<⁺ : (a : ℤ⁺) → Acc _<_ (fst a) → Acc _<⁺_ a
acc<→acc<⁺ (a , _) (acc acc<a) = acc (λ { (a′ , 1≤a′) a′<a → acc<→acc<⁺ (a′ , 1≤a′) (acc<a a′ a′<a)})

<⁺-wf : WellFounded _<⁺_
<⁺-wf a = acc<→acc<⁺ a (<-wf (fst a))

------------------------------------------------------------
-- Division Algorithm
------------------------------------------------------------

--------------------------------------------------
-- DivMod, i.e. results of division algorithm

data DivMod (n d q r : ℕ) : Set where
  DM : (r + q * d ≡ n) → (r < d) → DivMod n d q r

divMod→0<d : {n d q r : ℕ} → DivMod n d q r → 0 < d
divMod→0<d (DM _ r<d) = ≤-<-trans zle r<d

----------------------------------------
-- First, relationship of DivMod to ∣

mod0→divides : (n d : ℕ) {q : ℕ} → DivMod n d q 0 → d ∣ n
mod0→divides n d {q} (DM eq _) = q , eq

divides→mod0 : (n d : ℕ) → (0 < d) → d ∣ n → Σ ℕ (λ q → DivMod n d q 0)
divides→mod0 n d 0<d (q , qd≡n) = q , (DM qd≡n 0<d)

----------------------------------------
-- DivMod results are unique!

divModUnique : {n d q₁ r₁ q₂ r₂ : ℕ} → DivMod n d q₁ r₁ → DivMod n d q₂ r₂ → (q₁ ≡ q₂) × (r₁ ≡ r₂)
divModUnique {n} {d} {q₁} {r₁} {q₂} {r₂} dm@(DM r₁+q₁d≡n r₁<d) (DM r₂+q₂d≡n r₂<d) = q₁≡q₂ , r₁≡r₂
 where
  rem-diff : ∥ r₁ - r₂ ∥ ≡ ∥ q₁ * d - q₂ * d ∥
  rem-diff = sub₂ {r₁} (trans r₁+q₁d≡n (sym r₂+q₂d≡n))

  d∣r₁-r₂ : d ∣ ∥ r₁ - r₂ ∥
  d∣r₁-r₂ = ∥ q₁ - q₂ ∥ ,
    (begin
      ∥ q₁ - q₂ ∥ * d         ≡⟨ diff-distribʳ q₁ q₂ d ]≡
      ∥ q₁ * d - q₂ * d ∥     ≡⟨ rem-diff ]≡
      ∥ r₁ - r₂ ∥             ∎
    )

  -- Since r₁ < d and r₂ < d, then ∥ r₁ - r₂ ∥ < d;
  -- but since d ∣ ∥ r₁ - r₂ ∥, therefore ∥ r₁ - r₂ ∥ = 0, which means r₁ ≡ r₂.
  r₁≡r₂ : r₁ ≡ r₂
  r₁≡r₂ = diff0→≡ (∣<→0 d∣r₁-r₂ (diff-< r₁<d r₂<d))

  -- q₁ ≡ q₂ then follows.  First we show dq₁ = dq₂.
  dq₁≡dq₂ : d * q₁ ≡ d * q₂
  dq₁≡dq₂ = +-cancelˡ r₁ (d * q₁) (d * q₂)
    (begin
      r₁ + d * q₁             ≡[ r₁ +_ $≡ *-comm d q₁ ⟩≡
      r₁ + q₁ * d             ≡[ r₁+q₁d≡n ⟩≡
      n                       ≡⟨ r₂+q₂d≡n ]≡
      r₂ + q₂ * d             ≡[ _+_ $≡ sym r₁≡r₂ ≡$≡ *-comm q₂ d ⟩≡
      r₁ + d * q₂             ∎
    )

  -- Now we can finally show q₁ = q₂.
  q₁≡q₂ : q₁ ≡ q₂
  q₁≡q₂ = *-cancelˡ d q₁ q₂ (<→≢ (divMod→0<d dm)) dq₁≡dq₂

----------------------------------------
-- We can then use uniqueness to prove contrapositive of lemma about ∣

-- This is true because if d ∣ n, then we get a mod of 0, but divMod results are unique,
-- which would be a contradiction.
modS→¬divides : (n d : ℕ) {q r : ℕ} → DivMod n d q (suc r) → ¬ (d ∣ n)
modS→¬divides n d dm d∣n with divides→mod0 n d (divMod→0<d dm) d∣n
... | q₂ , dm₂ with divModUnique dm dm₂
... | q₁≡q₂ , ()

--------------------------------------------------
-- Division Algorithm proper

data Cmp (a d : ℕ) : Set where
  LT : a < d → Cmp a d
  GE : (a′ : ℕ) → (a′ + d ≡ a) → Cmp a d

_decreaseBy?_ : (a d : ℕ) → Cmp a d
zero decreaseBy? zero = GE 0 refl
zero decreaseBy? suc d = LT (sle zle)
suc a decreaseBy? zero = GE (suc a) ((suc a) +0)
suc a decreaseBy? suc d with a decreaseBy? d
... | LT a<d = LT (sle a<d)
... | GE a′ a′+d≡a = GE a′ (trans (a′ +suc d) (suc $≡ a′+d≡a))

incDivMod : {n′ n d q r : ℕ} → n′ + d ≡ n → DivMod n′ d q r → DivMod n d (suc q) r
incDivMod {n′} {n} {d} {q} {r} n′+d≡n (DM r+qd≡n′ r<d) = DM lem r<d
 where
  lem : r + (d + q * d) ≡ n
  lem = begin
    r + (d + q * d)           ≡⟨ +-assoc r _ _ ]≡
    (r + d) + q * d           ≡[ _+_ $≡ +-comm r _ ≡$ q * d ⟩≡
    (d + r) + q * d           ≡[ +-assoc d _ _ ⟩≡
    d + (r + q * d)           ≡[ d +_ $≡ r+qd≡n′ ⟩≡
    d + n′                    ≡[ +-comm d _ ⟩≡
    n′ + d                    ≡[ n′+d≡n ⟩≡
    n                         ∎

DivAlg : ℕ → ℕ → Set
DivAlg n d = Σ (ℕ × ℕ) (λ { (q , r) → DivMod n d q r })

-- Division algorithm, via well-founded induction!
divAlg : (n d : ℕ) → (0 < d) → DivAlg n d
divAlg n d 0<d = wf-ind {P = λ n → DivAlg n d} <-wf go n
 where
  go : (n : ℕ) → ((n′ : ℕ) → n′ < n → DivAlg n′ d) → DivAlg n d
  go n IH with n decreaseBy? d
  ... | LT n<d = (0 , n) , DM (n +0) n<d
  ... | GE n′ n′+d≡n with IH n′ (+→< 0<d n′+d≡n)
  ... | (q , r) , dm = (suc q , r) , incDivMod n′+d≡n dm

_∣?_ : (x y : ℕ) → Dec (x ∣ y)
zero ∣? zero = yes (0 , refl)
zero ∣? (suc y) = no λ { (a , eq) → absurd (noConf (trans (sym (*-comm a zero)) eq))}
(suc x) ∣? y with divAlg y (suc x) (sle zle)
... | (q , zero) , dm = yes (mod0→divides y (suc x) dm)
... | (q , suc r) , dm = no (modS→¬divides y (suc x) dm)

------------------------------------------------------------
-- Loops
------------------------------------------------------------

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

------------------------------------------------------------
-- Lists
------------------------------------------------------------

data List (A : Set) : Set where
  [] : List A
  _∷_ : A → List A → List A

All : (P : A → Set) → List A → Set
All P [] = ⊤
All P (x ∷ xs) = P x × All P xs

foldr : (A → B → B) → B → List A → B
foldr _&_ z [] = z
foldr _&_ z (x ∷ xs) = x & foldr _&_ z xs

_++_ : List A → List A → List A
xs ++ ys = foldr (_∷_) ys xs

product : List ℕ → ℕ
product = foldr _*_ 1

All-++ : {P : A → Set} {xs ys : List A} → All P xs → All P ys → All P (xs ++ ys)
All-++ {xs = []} Pxs Pys = Pys
All-++ {xs = x ∷ xs} (Px , Pxs) Pys = Px , All-++ Pxs Pys

product-++ : (xs ys : List ℕ) → product (xs ++ ys) ≡ product xs * product ys
product-++ [] ys = sym (_ +0)
product-++ (x ∷ xs) ys = trans (x *_ $≡ product-++ xs ys) (sym (*-assoc x (product xs) (product ys)))

------------------------------------------------------------
-- FTA
------------------------------------------------------------

FTA : ℤ⁺ → Set
FTA (n , _) = Σ (List ℕ) (λ ps → All Prime ps × product ps ≡ n)

-- FTA, without uniqueness.
fta : (n : ℤ⁺) → FTA n
fta n = wf-ind {P = FTA} <⁺-wf go n
 where
  go : (n : ℤ⁺) → ((n′ : ℤ⁺) → n′ <⁺ n → FTA n′) → FTA n
  go (suc zero , _) _ = [] , (tt , refl)
  go (suc (suc n) , 1≤n) IH with prime? (suc (suc n)) (sle (sle zle))
  ... | inj₁ P = (suc (suc n) ∷ []) , ((P , tt) , (suc $≡ (suc $≡ (n *1))))
  ... | inj₂ C with factors _ C
  ... | ((a , 2≤a , a<n , _) , (b , 2≤b , b<n , _)) , ab≡n
      with IH (a , ≤-trans (sle zle) 2≤a) a<n
         | IH (b , ≤-trans (sle zle) 2≤b) b<n
  ... | ps₁ , Pps₁ , prod₁ | ps₂ , Pps₂ , prod₂ = (ps₁ ++ ps₂) , (All-++ Pps₁ Pps₂) ,
    begin
      product (ps₁ ++ ps₂)      ≡[ product-++ ps₁ ps₂ ⟩≡
      product ps₁ * product ps₂ ≡[ _*_ $≡ prod₁ ≡$≡ prod₂ ⟩≡
      a * b                     ≡[ ab≡n ⟩≡
      suc (suc n)               ∎
```
