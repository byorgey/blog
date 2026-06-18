---
title: 'Proving the Fundamental Theorem of Arithmetic in Agda'
categories: Agda
katex: true
tags: agda,arithmetic,theorem,proof
---

**tl;dr**: This is a fully commentated, from-scratch proof of the
Fundamental Theorem of Arithmetic in
[Agda](https://wiki.portal.chalmers.se/agda/pmwiki.php?n=Main.HomePage),
intended for those who already know a bit of Agda but might benefit
from reading and working through a larger example.  See the
Introduction and the [Table of Contents](#table-of-contents) below for more details.

**This version of the file has most of the proofs replaced by Agda
holes, so you can try to fill in the proofs for yourself.**

## Introduction

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

So I decided to spend about an hour trying to prove it in Agda,
to gauge the level of the project.  At the end of an hour, I had
learned two things: (1) proving the Fundamental Theorem of Arithmetic is *not* an appropriate project for
my students (who had only had a few weeks' practice with Agda); (2) I
was not going to be able to stop until I finished the proof myself!

Over the next week or so, I finished the proof completely from
scratch—without using anything from the standard library, and without
looking up any reference material.  I based it only on my experience
in Agda, knowledge of the relevant proofs on an informal level, and
Agda tricks I've picked up along the way (from *e.g.* Conor McBride, Jacques
Carette, colleagues at Penn, and elsewhere).

I decided to publish the proof, with extra commentary, in the hopes
that it can be useful as an intermediate-level reference.  That is,
perhaps you've learned some basic Agda (if not, I suggest [this
tutorial to
start](https://agda.readthedocs.io/en/latest/getting-started/a-taste-of-agda.html))
and have some basic familiarity
with the Curry-Howard correspondence, but would benefit from seeing an
example of a fully worked out, medium-sized proof.^[Another good
source of information along these lines is [this post by Jesper
Cockx](https://jesper.sikanda.be/posts/formalize-all-the-things.html).] The resulting blog
post is extremely long, but I make no apologies for that—if you want
an entertaining 5-minute read, you should look elsewhere!

The entire blog post and proof is [available as a literate Agda
document](https://github.com/byorgey/blog/blob/main/posts/2026/06/26/FTA.lagda.md).  Even better, I have also published [another version of this
blog post with holes in place of almost all the proofs](https://github.com/byorgey/blog/blob/main/posts/2026/06/26/FTA-holes.lagda.md).  For a
maximal learning experience, I suggest downloading it and trying to
fill in the holes yourself as you go along.

Below is a table of contents.  Depending on your background, you may
of course choose to skip some sections.  For example, if you have
already had a good deal of practice dealing with basic natural number
arithmetic, equality, and inequality in Agda, you might wish to skip
over those sections.

## Table of Contents

* [Introduction](#introduction)
* [(Half of) The Fundamental Theorem of Arithmetic (Constructively)](#half-of-the-fundamental-theorem-of-arithmetic-constructively)
* [Preliminaries](#preliminaries)
* [Basic logic](#basic-logic)
* [Equality](#equality)
* [Natural numbers](#natural-numbers)
    * [No confusion](#no-confusion)
    * [Decidable equality](#decidable-equality)
* [Addition](#addition)
* [Multiplication](#multiplication)
* [Inequality](#inequality)
    * [Relationships among equality and inequality](#relationships-among-equality-and-inequality)
    * [Arithmetic and inequality](#arithmetic-and-inequality)
* [Divisibility, primes, and composites](#divisibility-primes-and-composites)
    * [Nontrivial divisors come in pairs](#nontrivial-divisors-come-in-pairs)
* [Division](#division)
* [Absolute difference](#absolute-difference)
* [Quotient and remainder are unique](#quotient-and-remainder-are-unique)
* [The division algorithm, take 1](#the-division-algorithm-take-1)
    * [Construct the evidence you would like to pattern-match on](#construct-the-evidence-you-would-like-to-pattern-match-on)
* [Well-founded induction](#well-founded-induction)
    * [Accessibility](#accessibility)
    * [Well-founded induction, defined](#well-founded-induction-defined)
    * [Less-than is well-founded](#less-than-is-well-founded)
* [The division algorithm](#the-division-algorithm)
* [Primality testing](#primality-testing)
    * [Counting up](#counting-up)
    * [Primality testing by trial division](#primality-testing-by-trial-division)
* [Lists](#lists)
* [The Fundamental Theorem of Arithmetic](#the-fundamental-theorem-of-arithmetic)
* [Further Directions](#further-directions)

## (Half of) The Fundamental Theorem of Arithmetic (Constructively)

The [Fundamental Theorem of
Arithmetic](https://en.wikipedia.org/wiki/Fundamental_theorem_of_arithmetic)
(*FTA* for short) states that any natural number $n \geq 1$ can be
written as a product of zero or more primes, and moreover that this
product is unique up to permutation.

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
names as they do in the [Agda standard library](https://agda.github.io/agda-stdlib/v2.3/).  However, many things
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
¬ P = {!!}
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
fst = {!!}

infixr 3 _×_
_×_ : (A B : Set) → Set
A × B = {!!}
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
sym = {!!}

trans : {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans = {!!}

cong : (f : A → B) → {x y : A} → x ≡ y → f x ≡ f y
cong = {!!}
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
The intention is that this proof shows `v ≡ z`, by first using `p` to
show that `v ≡ w`, then `q` to show `w ≡ x`, and so on.  This notation
is one of my favorite applications of Agda's [mixfix operator
syntax](https://agda.readthedocs.io/en/latest/language/mixfix-operators.html),
and has several benefits:

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
begin_ = {!!}

infixr 2 _≡[_⟩≡_
_≡[_⟩≡_ : (x : A) → {y z : A} → (x ≡ y) → (y ≡ z) → (x ≡ z)
_≡[_⟩≡_ = {!!}

infixr 2 _≡⟨_]≡_
_≡⟨_]≡_ : (x : A) → {y z : A} → (y ≡ x) → (y ≡ z) → (x ≡ z)
_≡⟨_]≡_ = {!!}

infixr 5 _∎
_∎ : (x : A) → x ≡ x
_∎ = {!!}
```

Finally, a few [Applicative](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Prelude.html#t:Applicative)-like operators for more conveniently
writing common forms of congruence.  For example, instead of writing
`cong f x≡y`, we can write `f $≡ x≡y`; or to use congruence on both
arguments of a two-place function at once, we can write `f $≡ x≡y ≡$≡
z≡w`.  (These operators were also inspired by Conor.)

```agda
infixl 4 _$≡_
_$≡_ : (f : A → B) → {x y : A} → x ≡ y → f x ≡ f y
_$≡_ = {!!}

infixl 4 _≡$_
_≡$_ : {f g : A → B} → f ≡ g → (x : A) → f x ≡ g x
_≡$_ = {!!}

infixl 4 _≡$≡_
_≡$≡_ : {f g : A → B} → f ≡ g → {x y : A} → x ≡ y → f x ≡ g y
_≡$≡_ = {!!}
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
confusion".  This property and its name is well-known in the
literature; for example, see
[McBridge](http://strictlypositive.org/concon.ps.gz) or [Cornes +
Terrasse](https://link.springer.com/chapter/10.1007/3-540-61780-9_64).

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
NoConf = {!!}
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
noConf = {!!}
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
[law of excluded middle](https://en.wikipedia.org/wiki/Law_of_excluded_middle), which
says that $P \lor \neg P$ for all propositions $P$, is rejected in constructive logic.  However, even
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
_≟_ = {!!}
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
_+_ = {!!}

_+0 : (n : ℕ) → (n + 0 ≡ n)
_+0 = {!!}

_+suc_ : (x y : ℕ) → (x + suc y) ≡ suc (x + y)
_+suc_ = {!!}

+-comm : (x y : ℕ) → x + y ≡ y + x
+-comm = {!!}

+-assoc : (x y z : ℕ) → (x + y) + z ≡ x + (y + z)
+-assoc = {!!}

+-cancelˡ : (x y z : ℕ) → x + y ≡ x + z → y ≡ z
+-cancelˡ = {!!}
```

## Multiplication

Multiplication is next: we start by defining the multiplication
operation (by pattern-matching on the left-hand argument) and proving
a few lemmas about multiplying by known arguments on the right. The
proof of `*suc` is the most involved proof we have seen yet, but it
ultimately just comes down to algebra, and we can make good use of our
notation for writing chained equality proofs.

```agda
infixl 7 _*_
_*_ : ℕ → ℕ → ℕ
_*_ = {!!}

_*0 : (n : ℕ) → (n * 0 ≡ 0)
_*0 = {!!}

_*1 : (n : ℕ) → (n * 1 ≡ n)
_*1 = {!!}

_*suc_ : (x y : ℕ) → (x * suc y ≡ x + x * y)
_*suc_ = {!!}
```

We prove some standard properties of multiplication: commutativity,
distributivity over addition, associativity.  Again, the proofs mostly
consist of a whole bunch of algebra, using the special notation for
building chained equality proofs.

```agda
*-comm : (x y : ℕ) → x * y ≡ y * x
*-comm = {!!}

*-distribˡ : (x y z : ℕ) → x * (y + z) ≡ x * y + x * z
*-distribˡ = {!!}

*-distribʳ : (x y z : ℕ) → (x + y) * z ≡ x * z + y * z
*-distribʳ = {!!}

*-assoc : (x y z : ℕ) → (x * y) * z ≡ x * (y * z)
*-assoc = {!!}
```

Finally, we prove that multiplication is left-cancellative.  This
proof is somewhat tricky—in the case that `x`, `y`, and `z` are all
successors, we need to use the induction hypothesis (*i.e.* a
recursive call to `*-cancelˡ`) on `x` and the
predecessors of `y` and `z`, using the fact that + is
left-cancellative to construct the required input equality.

```agda
*-cancelˡ : (x y z : ℕ) → (0 ≢ x) → x * y ≡ x * z → y ≡ z
*-cancelˡ = {!!}
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
≤-refl = {!!}

≤-trans : {x y z : ℕ} → x ≤ y → y ≤ z → x ≤ z
≤-trans = {!!}

≤-sucr : {m n : ℕ} → m ≤ n → m ≤ suc n
≤-sucr = {!!}

≤-sucl : {m n : ℕ} → suc m ≤ n → m ≤ n
≤-sucl = {!!}

≤-pred : {x y : ℕ} → suc x ≤ suc y → x ≤ y
≤-pred = {!!}
```

For convenience, we define $<$ in terms of $\leq$, and prove a few
properties: any number is less than its successor, and $<$ is
transitive and non-reflexive.

```agda
_<_ : ℕ → ℕ → Set
x < y = suc x ≤ y

_<suc : (x : ℕ) → x < suc x
_<suc = {!!}

<-trans : {x y z : ℕ} → x < y → y < z → x < z
<-trans = {!!}

x≮x : {x : ℕ} → ¬ (x < x)
x≮x = {!!}
```

### Relationships among equality and inequality

Of course, equality, $<$ and $\leq$ have various relationships that we
will need.  First, equality implies $\leq$.

```agda
≡→≤ : {x y : ℕ} → x ≡ y → x ≤ y
≡→≤ = {!!}
```

Next, $x < y$ implies that $x$ and $y$ are *not* related by $\equiv$ or
$\geq$.  The first lemma in particular—that $<$ implies $\not\equiv$—gets used quite a bit.  Note that it can be read in two equivalent
ways: on the surface, it is a way to turn a proof of $x < y$ into a
proof of $x \not\equiv y$; but
since $x \not\equiv y$ is really an abbreviation for $(x \equiv y) \to \bot$, it can be
used to derive a contradiction if we have proofs that $x < y$ and
also $x \equiv y$.

```agda
<→≢ : {x y : ℕ} → x < y → x ≢ y
<→≢ = {!!}

<→≱ : {x y : ℕ} → x < y → ¬ (y ≤ x)
<→≱ = {!!}
```

If $x \leq y$ but they are not equal, then $x < y$.

```agda
≤≢→< : {x y : ℕ} → x ≤ y → x ≢ y → x < y
≤≢→< = {!!}
```

We will need a form of transitivity that says if $x \leq y$ and $y <
z$, then $x < z$, as well as the other way around.

```agda
≤-<-trans : {x y z : ℕ} → x ≤ y → y < z → x < z
≤-<-trans = {!!}

<-≤-trans : {x y z : ℕ} → x < y → y ≤ z → x < z
<-≤-trans = {!!}
```

Finally, a very specific lemma we will need: if a number is not
equal to either 0 or 1, then it must be greater than or equal to 2.

```agda
¬01-is-≥2 : (a : ℕ) → (a ≢ 0) → (a ≢ 1) → (2 ≤ a)
¬01-is-≥2 = {!!}
```

### Arithmetic and inequality

The last lemmas we need relate arithmetic operations and inequality.
First, adding and multiplying cannot make anything smaller (unless we
multiply by zero, of course).

```agda
≤+ : {x y : ℕ} → x ≤ (x + y)
≤+ = {!!}

≤* : {x y : ℕ} → (x ≢ 0) → y ≤ (x * y)
≤* = {!!}
```

As a result, if we know that one thing is equal to a sum or product of
other things, we can conclude something about their relative sizes.

```agda
+→≤ : {x y z : ℕ} → x + y ≡ z → x ≤ z
+→≤ = {!!}

+→< : {x y z : ℕ} → 0 < y → x + y ≡ z → x < z
+→< = {!!}

*→≤ : {x y z : ℕ} → (y ≢ 0) → y * x ≡ z → x ≤ z
*→≤ = {!!}
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
Prime n = (2 ≤ n) × (∀ (d : ℕ) → (d < n) → (2 ≤ d) → ¬ (d ∣ n))
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
  {!!}
```

First, we need a lemma that $0 < n$, which follows because $0 < 1 < a
< n$ (remember that a proof of $1 < a$ is actually defined to be the
same thing as a proof of $2 \leq a$).

```agda
 where
  0<n : 0 < n
  0<n = {!!}
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
    {!!}
```

If $b$ were $1$, then $ba = n$ would imply $a = n$, but we know
$a < n$, so this is also a contradiction.

```agda
    {!!}
```

Finally, we prove $b < n$, by showing $b \leq n$ and $b \neq n$.

```agda
  b<n : b < n
  b<n = ≤≢→<
```

$b \leq n$ since $ba = n$ and $a$ is not zero (if $a$ were zero it
would contradict the fact that $2 \leq a$).

```agda
    {!!}
```

$b \neq n$, since $b = n$ together with $ba = n$ would imply $a = 1$
(since multiplication is cancellative), but $2 \leq a$ so it cannot
equal 1.

```agda
    {!!}
```

## Division

Let's start working our way towards proving that divisibility is
decidable.  To check whether $d \mid n$, the usual idea would be to
divide $n$ by $d$ and check whether we get a remainder of zero.  So we
need to formalize this notion of division with remainder.

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
divMod→0<d = {!!}
```

We can also show that for nonzero $d$, the remainder is zero if and only if $d
\mid n$:

```agda
mod0→divides : (n d : ℕ) {q : ℕ} → DivMod n d q 0 → d ∣ n
mod0→divides = {!!}

divides→mod0 : (n d : ℕ) → (0 < d) → d ∣ n → Σ ℕ (λ q → DivMod n d q 0)
divides→mod0 = {!!}
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
∥_-_∥ = {!!}
```

Of course, we will need a lot of small lemmas about the properties of
this operation.  We can start by proving that the distance between two
numbers is 0 if and only if they are equal:

```agda
diff0 : (x : ℕ) → 0 ≡ ∥ x - x ∥
diff0 = {!!}

diff0→≡ : {x y : ℕ} → 0 ≡ ∥ x - y ∥ → x ≡ y
diff0→≡ = {!!}
```

Next, the distance between any number and 0 is the number itself, and
the distance function is commutative.
```agda
∥x-0∥≡x : (x : ℕ) → ∥ x - 0 ∥ ≡ x
∥x-0∥≡x = {!!}

diff-comm : {x y : ℕ} → ∥ x - y ∥ ≡ ∥ y - x ∥
diff-comm = {!!}
```

A key lemma supporting the argument outlined in the previous section
is that if $x$ and $y$ are both less than $d$, so is their absolute difference.

```agda
diff-< : {x y d : ℕ} → x < d → y < d → ∥ x - y ∥ < d
diff-< = {!!}
```

We can also cancel the same thing being added to both sides, or factor out
the same thing being multiplied on both sides.

```agda
diff-cancelˡ : (a b c : ℕ) → ∥ (a + b) - (a + c) ∥ ≡ ∥ b - c ∥
diff-cancelˡ = {!!}

diff-distribʳ : (x y d : ℕ) → ∥ x * d - y * d ∥ ≡ ∥ x - y ∥ * d
diff-distribʳ = {!!}
```

Another key lemma is that if $w + x = y + z$, then $\|w - y\| = \|x -
z\|$ (`sub₂` below). Personally, I found this quite tricky to prove.
The best approach I found was to first prove the simpler lemma that
$x + y = z$ implies $x = \| z - y \|$ (`sub₁`), which can then be used
in several places in the proof of `sub₂`.

```agda
sub₁ : {x y z : ℕ} → x + y ≡ z → x ≡ ∥ z - y ∥
sub₁ = {!!}

sub₂ : {w x y z : ℕ} → w + x ≡ y + z → ∥ w - y ∥ ≡ ∥ x - z ∥
sub₂ = {!!}
```

## Quotient and remainder are unique

We can now return to prove that quotient and remainder are unique.
First, we show that zero is the only multiple of $d$ which is less than $d$.

```agda
∣<→0 : {d x : ℕ} → d ∣ x → x < d → 0 ≡ x
∣<→0 = {!!}
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
  rem-diff = {!!}
```
Next, we can show that $d$ divides the absolute difference $\|r_1 -
  r_2\|$, by factoring it out of $\|q_1 d - q_2 d\|$.
```agda
  d∣r₁-r₂ : d ∣ ∥ r₁ - r₂ ∥
  d∣r₁-r₂ = {!!}
```

We can then put three lemmas together to conclude $r_1 = r_2$: first,
since $r_1$ and $r_2$ are both less than $d$, so is their absolute
difference; since $d$ also divides the absolute difference, the
absolute difference must be zero; and finally, an absolute difference
of zero means $r_1$ and $r_2$ must be equal.

```agda
  r₁≡r₂ : r₁ ≡ r₂
  r₁≡r₂ = {!!}
```

From here, proving $q_1 = q_2$ just requires some algebra.

```agda
  dq₁≡dq₂ : d * q₁ ≡ d * q₂
  dq₁≡dq₂ = {!!}

  q₁≡q₂ : q₁ ≡ q₂
  q₁≡q₂ = {!!}
```

Finally, we can use uniqueness of quotients and remainders to show the
lemma we wanted about divisibility and remainders: if $n$ divided by
$d$ has some nonzero number as remainder, then $d$ does not divide
$n$.  If $d$ did divide $n$, then we know we would get a remainder of
$0$; but since remainders are unique, we can't have both a zero and
nonzero remainder.

```agda
modS→¬divides : (n d : ℕ) {q r : ℕ} → DivMod n d q (suc r) → ¬ (d ∣ n)
modS→¬divides = {!!}
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
_decreaseBy?_ = {!!}
```

We can also write a helper function `incDivMod` which encodes the
observation from before, that if $r + qd = n-d$, then $r + (q+1)d =
n$.  Of course we don't actually want to use subtraction, so instead
of writing $n-d$, we work in terms of an $n'$ such that $n' + d = n$.
Proving this requires only some straightforward algebra.

```agda
incDivMod : {n′ n d q r : ℕ} → n′ + d ≡ n → DivMod n′ d q r → DivMod n d (suc q) r
incDivMod = {!!}
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
this bad version of `divAlg` in the code without causing an error.)
The problem is that the recursive call to `divAlg` is on `n′`,
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

The correct type for `divAlg` is `(n d : ℕ) → (0 < d) → DivAlg n d`,
but we're still going to have trouble convincing Agda that our
algorithm is terminating.  In order to do so, we need to take a detour
through *well-founded induction*.

## Well-founded induction

Normally, Agda only allows functions that are *structurally
recursive*—that is, functions which make recursive calls on syntactic
subterms of their inputs.  (Agda's termination checking is a bit more
sophisticated than that, but that's the basic idea.)  However, we can
use basic structural induction to bootstrap our way into more exotic
forms.  In particular, we are going to define something called
*well-founded induction*.^[Note that I will use the terms "recursion"
and "induction" more or less interchangeably.  In some contexts, people
make a distinction between the terms (typically a *recursion
principle* is a less-dependently-typed version of an *induction
principle*), but in this context, *inductive proofs* correspond, via
Curry-Howard, to (suitably restricted) *recursive functions*, so
"induction" and "recursion" describe the same thing from a logical and
computational viewpoint, respectively.]

The idea of well-founded induction starts with the general idea of a
*relation*.  A relation on `A` is just a function that takes two
values of type `A` and produces a type, representing evidence that the
two values are related (according to whatever kind of relationship we
have in mind).
```agda
Rel : Set → Set₁
Rel A = A → A → Set
```
We have already seen quite a few relations, such as  equality, $<$,
$\leq$, and divisibility.

Suppose we're writing a recursive function, with some relation $\prec$
in mind, and for a given input $x$ we're only allowed to make
recursive calls on values $y$ such that $y \prec x$.  If $\prec$ is the
"is a syntactic subterm of" relation, then we get structural
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
data Acc (_≺_ : Rel A) : A → Set where
  acc : {x : A} → ((y : A) → y ≺ x → Acc _≺_ y) → Acc _≺_ x
```

`Acc _≺_ x` defines what it means for a particular value `x` to be
accessible with respect to a relation $\prec$.  There is only one
constructor, `acc`, which requires `(y : A) → y ≺ x → Acc _≺_ y`—that
is, for every value `y` of type `A`, if `y` is related to `x`, then
`y` is accessible.  In other words, `x` is accessible if and only if every `y ≺ x`
is accessible.

This is definitely tricky to wrap your head around!  At this point
you may have two objections:

1. What about base cases?  Shouldn't we have another constructor which
   says `x` is accessible if *nothing* is related to it?  Actually,
   the `acc` constructor already says that!  If nothing is related to
   `x`, then `(y : A) → y ≺ x → Acc _≺_ y` is trivially true: we can
   easily promise anything we want as the output of a function if we
   know it can never be called.  Every natural number less than zero is a
   purple flying weasel.

2. Doesn't this just run into the same problem as before with
   left-infinite chains?  If we consider the "is one less than"
   relation on the integers, isn't $2$ accessible because $1$ is
   accessible because $0$ is accessible because $-1$ is accessible
   because ... ?

   There is something a bit subtle going on here: recursive data types
   in Agda (unlike Haskell) are interpreted according to a
   *least fixed point* semantics.  Put in plain terms, the only values
   of a data type are those which can be built by applications of a
   *finite* number of constructors.  So in fact, the "no left-infinite
   chain" condition is foundationally built into the way Agda data
   types work!

The idea is that we can turn well-founded induction into structural
induction by *pattern-matching on `Acc` proofs*!  Starting with some
$x$, any $y \prec x$ has an accessibility proof which is a subterm of
the accessibility proof for $x$.  This is a bit exotic though: if we
pattern-match on the `acc` for $x$ we get a *function* that yields an
accessibility proof for each $y \prec x$; calling that function
produces another accessibility proof, which *counts as a structural
subterm of the original*.  This seems a bit strange until you think of
a value of type `Acc` like an big, arbitrarily-branching tree of
finite depth; each node contains a function which really just stores
all the subtrees.  In other words, a function of type `(y : A) → y ≺ x
→ Acc _≺_ y` can be thought of as a giant tuple of `Acc` values, one
for each `y \prec x`.^[I am not sure exactly how Agda handles this
internally, but I assume this is well-trodden ground for designers of
proof assistants.]

### Well-founded induction, defined

Given the definition of accessible elements, we can now give the
definition of a well-founded relation: a relation on `A` is well-founded if
*every* value of type `A` is accessible.

```agda
WellFounded : Rel A → Set
WellFounded {A} _≺_ = (a : A) → Acc _≺_ a
```

We can now write down the *principle of well-founded induction*.  This
is also quite tricky to wrap your brain around, so we'll go through it
slowly.  Previously, we were just talking about whether functions terminated or
not; but the reason this is important is that a function might be
calculating a *proof*.  A function which purports to calculate a proof
but sometimes goes into infinite recursion is a charlatan, and not
really a proof at all.

So instead of thinking about termination, let's switch to thinking
about proofs.  Given a proposition $P$, we want to prove that
$P$ holds for every value of type $A$.  The idea is that when trying to
prove $P(y)$ for a particular $y$, we get to assume that $P(z)$ holds
(*i.e.* we get to make recursive calls) for all $z \prec y$.

Here, then, is the statement of the principle of well-founded induction, with
each argument broken out on a separate line so we can explain them as
we go.
```agda
wf-ind :
  {P : A → Set}
```
* `P` represents an arbitrary proposition on `A`; our goal is to show `P` holds for
  every value of type `A`.
```agda
  {_≺_ : Rel A} →
```
* An arbitrary relation.
```agda
  WellFounded _≺_ →
```
* A proof that `≺` is a well-founded relation.
```agda
  ((y : A) → ((z : A) → z ≺ y → P z) → P y) →
```
* This is the trickiest
  argument to understand.  Intuitively, it says "For any
  `y`, if we know `P z` holds for all `z ≺ y`, then we can show `P y`
  also holds."
```agda
  (x : A) → P x
```
* The principle of well-founded induction says that all of this is
  enough to show that `P x` holds for *all* `x : A`.

So, how do we implement this?  If we try something
straightforward, as in `wf-ind-bad` below—just call `ind` on `x`, then call `wf-ind` recursively
to fill in the proofs for `P z`—of course it does not work; Agda cannot
tell that this is terminating.^[As an aside, Agda complains that this definition of `wf-ind-bad`
is not terminating—which makes sense—but it continues to complain even
when I include the `NON_TERMINATING` pragma, which I don't
understand.  Perhaps this has been fixed in a more recent version of Agda.]  And this makes sense, because we are
not even using the fact that the relation is well-founded at all!

```plain
module WFIndBad where

  {-# NON_TERMINATING #-}
  wf-ind-bad : {P : A → Set} {_≺_ : Rel A} → WellFounded _≺_ → ((y : A) → ((z : A) → z ≺ y → P z) → P y) → (x : A) → P x
  wf-ind-bad wf ind x = ind x (λ z Rzx → wf-ind-bad wf ind z)
```

Instead, the right idea is to use the fact that $\prec$ is
well-founded to generate an initial proof of accessibility for the
input $x$, and then *pattern match on accessibility proofs* alongside
the values as we recurse.  Every time we make a recursive call on some
$y \prec x$, we can just pattern-match on the accessibility proof for
$x$ to get an accessibility proof for $y$, so Agda will be able to see
that the whole thing is *structurally* recursive on the accessibility
proofs.

```agda
wf-ind = {!!}
```

### Less-than is well-founded

Now that we have well-founded induction under our belts, let's show
that the less-than relation on natural numbers is well-founded. This
corresponds to what is often called "strong induction".^[Incidentally,
we could probably have gotten away with directly defining a principle
of strong natural number induction, without bothering with the full
generality of well-founded induction, but this way is more fun and
interesting!] To prove that $<$ is well-founded, we of course must
show that every natural number is accessible under $<$.  However, if
we directly try to prove `(m : ℕ) → (Acc _<_) m`, we run into a
variant of the exact same problem we have been dealing with: to prove
that $m$ is accessible we need to know that *every* $k < m$ is also
accessible, but again, we cannot show this directly by
recursion/induction, since $k$ may not be a structural subterm of $m$.

What we need is the usual trick for proving strong induction from weak
induction: instead of proving that $P(x)$ holds for all $x$, we prove
that $(\downarrow P)(x)$ holds for all $x$, where $\downarrow P$ is
the "downward closure" of $P$.  That is, $(\downarrow P)(x)$ says that
$P(x)$ holds for *all* $y < x$.^[Note that another way to define
$(\downarrow P)(x)$ is that $P(y)$ holds for all $y \sim x$, where $\sim$
is the *transitive closure* of the predecessor relation.  As an
advanced exercise, generalize $\downarrow_\prec P$ to be defined relative to
the transitive closure of any relation $\prec$, and then prove that $\prec$ is well-founded
if and only if its transitive closure is. For more along
these lines, see this [very cool (but much more abstract) post on
well-founded induction by Callan McGill](https://boarders.github.io/posts/well_founded_induction.html).]

```agda
↓ : (ℕ → Set) → (ℕ → Set)
↓ P n = (k : ℕ) → (k < n) → P k
```

Now we can prove, for all natural numbers $m$, that every natural
number up to and including $m$ is accessible under the $<$ relation.  Zero is accessible
because nothing is less than it; everything up to the successor of $m$ is accessible because by induction we know everything up to $m$ is, and anything less than the successor of $m$ must in fact be $\leq m$.

```agda
<-acc : (m : ℕ) → ↓ (Acc _<_) m
<-acc = {!!}
```

Finally, to show that any natural number $n$ is accessible—*i.e.* that $<$
is well-founded—we can use the fact that all numbers less than the successor of $n$
are accessible, and just project out accessibility for $n$ itself.

```agda
<-wf : WellFounded _<_
<-wf = {!!}
```

## The division algorithm

Finally, we can define the division algorithm, via well-founded
induction!  The definition is very similar to our first attempt, but we use the principle of well-founded induction with $<$.  Note that neither `divAlg` nor its helper function `go` is directly recursive. Instead, `go` takes an induction hypothesis as an argument, which we call instead, providing an extra proof that the subject of the induction hypothesis is in fact less than the original input.  `wf-ind` takes care of the actual recursion.

```agda
divAlg : (n d : ℕ) → (0 < d) → DivAlg n d
divAlg = {!!}
```

Using the division algorithm, we can also finally decide whether one
number divides another: zero divides zero; zero does not divide any
successor since that would imply there is some $k$ such that $k$ times
zero is nonzero, which is absurd; and if $x$ is a successor, we can
apply the division algorithm and check the remainder, applying some
previous lemmas that say what zero and nonzero remainders tells us
about divisibility.

```agda
_∣?_ : (x y : ℕ) → Dec (x ∣ y)
_∣?_ = {!!}
```

## Primality testing

To test a number for primality, we are just going to use
straightforward, naive trial division.  The straightforward way of
doing this is by starting at $2$ and counting *up*—but this is a
problem, because when pattern-matching on natural numbers we most
naturally count *down*.

Well, remember—build the evidence you want to pattern-match on!  Let's
develop some machinery for counting up instead of down.

### Counting up

The problem starts with $\leq$: a proof of $m \leq n$ starts with a
base case representing evidence that $0 \leq k$, then every
constructor application of `sle` increments both sides by one.
Pattern-matching on a proof of $m \leq n$ thus either reveals that $m
= 0$, or that $m' \leq n'$ where $m'$ and $n'$ are the predecessors of
$m$ and $n$: in other words, it facilitates counting *down*, just
like matching directly on $m$ would.

However, there is an alternative way to define the $\leq$ relation,
which we will call $\leq'$.
We can choose *reflexivity* of $\leq'$ as a base case, that is, $n \leq'
n$ for any $n$.  We can then *decrement* the left-hand side every time
we apply another constructor.  Like so:

```agda
data _≤′_ : ℕ → ℕ → Set where
  lerefl : {n : ℕ} → n ≤′ n
  lesuc : {m n : ℕ} → suc m ≤′ n → m ≤′ n
```

Pattern-matching on a proof of $m \leq' n$ thus facilitates counting
*up* from $m$ to $n$, just like we wanted!

We can also prove a few lemmas about properties of $\leq'$.  For
example, the two axioms that define the usual $\leq$ can be proved as
lemmas.


```agda
≤′-suc : {m n : ℕ} → m ≤′ n → suc m ≤′ suc n
≤′-suc = {!!}

0≤′ : (n : ℕ) → 0 ≤′ n
0≤′ = {!!}
```

We can also prove that $m \leq n$ implies $m \leq' n$.  (The converse
is true as well, and can be proved as an easy exercise, but we won't need it.)

```agda
≤→≤′ : {m n : ℕ} → m ≤ n → m ≤′ n
≤→≤′ = {!!}
```

So, we can pattern-match on a proof of $m \leq' n$ to count up from $m$ to
$n$, but this isn't quite good enough: while counting, we won't remember the relationship of the
intermediate values to the original $m$.  We would like to be able to
count up through some interval, from some starting $a$ to ending $b$,
knowing all along the way that the values we count are contained in
the interval.

To this end, we can create a data
type `i ∈[ a ⋯ b ]` which represents a stage in counting from $a$ to
$b$.  The base case is when $i = b$; otherwise `i ∈[ a ⋯ b ]` when $a
\leq i$ and also `suc i ∈[ a ⋯ b ]`.

```agda
data _∈[_⋯_] : ℕ → ℕ → ℕ → Set where
  stop : {a b : ℕ} → a ≤ b → b ∈[ a ⋯ b ]
  step : {a i b : ℕ} → a ≤ i → suc i ∈[ a ⋯ b ] → i ∈[ a ⋯ b ]
```

We can then write a function which "constructs a loop"—that is,
starting from a proof of $a \leq' b$, it builds
a value of type `a ∈[ a ⋯ b ]` which represents a reified loop from
$a$ to $b$.  By pattern-matching on this value we can successively
increment from $a$ up to $b$, with the appropriate guarantees along
the way.

```agda
loop : (a b : ℕ) → (a ≤′ b) → a ∈[ a ⋯ b ]
loop = {!!}
```

Finally, we need as a simple lemma the fact that if `i ∈[ a ⋯ b ]`
then in fact $i \leq b$.

```agda
top : {i a b : ℕ} → i ∈[ a ⋯ b ] → i ≤ b
top = {!!}
```

### Primality testing by trial division

First, a lemma about downward closure: if we know $(\downarrow P)(m)$,
and we know $P(m)$, then we know $(\downarrow P)(1 + m)$.  In other
words, if $P$ holds for everything less than $m$, we can extend it by
one by providing a proof that $P$ also holds for $m$.

```agda
extend : {P : ℕ → Set} {m : ℕ} → (↓ P) m → P m → (↓ P) (suc m)
extend = {!!}
```

Now we can define primality testing itself, via trial division.  We
`loop` from $2$ up to $n$, testing each number to see if it divides
$n$, keeping track along the way of the fact that all the numbers less
than our current trial divisor do not divide $n$.  If the next divisor
does divide $n$, we return it as proof that $n$ is composite.  If it
does not, we `extend` our accumulating proof of all the numbers that do
not divide $n$, and proceed to the next.  If we reach $n$, our
accumulated proof tells us that none of the numbers less than $n$
divide it, which is proof that $n$ is prime.

```agda
prime? : (n : ℕ) → (2 ≤ n) → Prime n ⊎ Composite n
prime? n 2≤n = trialDiv {!!} noDivisorsUpTo2
 where
  NoDivisorsUpTo : ℕ → Set
  NoDivisorsUpTo = ↓ (λ # → (2 ≤ #) → ¬ (# ∣ n))

  noDivisorsUpTo2 : NoDivisorsUpTo 2
  noDivisorsUpTo2 = {!!}

  trialDiv : {m : ℕ} → m ∈[ 2 ⋯ n ] → NoDivisorsUpTo m → Prime n ⊎ Composite n
  trialDiv = {!!}
```

## Lists

Before we are able to state the Fundamental Theorem of Arithmetic, we need to build up a data type for lists, along with some standard list manipulation functions.  First, we define the type of lists and the standard `foldr` function.

```agda
data List (A : Set) : Set where
  [] : List A
  _∷_ : A → List A → List A

foldr : (A → B → B) → B → List A → B
foldr = {!!}
```

Now we can define concatenation and product via `foldr`.

```agda
_++_ : List A → List A → List A
_++_ = {!!}

product : List ℕ → ℕ
product = {!!}
```

We will need `All`, which expresses that some predicate holds of all
the elements of a list.  In fact, `All` is manifestly an instance of
`foldr` as well, but we would need a universe-polymorphic version of `foldr` for that, so we just write it manually.

```agda
All : (P : A → Set) → List A → Set
All = {!!}
```

Now, we just need a couple lemmas about concatenation: first, that if `P` holds for all the elements in `xs` and all the elements in `ys`, then it holds for all the elements in `xs ++ ys`; and second, that `product` distributes over concatenation (*i.e.* it is a homomorphism from the monoid of lists under concatenation to the monoid of natural numbers under multiplication).

```agda
All-++ : {P : A → Set} {xs ys : List A} → All P xs → All P ys → All P (xs ++ ys)
All-++ = {!!}

product-++ : (xs ys : List ℕ) → product (xs ++ ys) ≡ product xs * product ys
product-++ = {!!}
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
fta n = wf-ind {P = {!!}} {!!} go n
 where
  go : (n : ℕ) → ((n′ : ℕ) → n′ < n → 0 < n′ → FTA n′) → 0 < n → FTA n
  go = {!!}
```

## Further Directions

That concludes our tour of the Fundamental Theorem of Arithmetic in Agda! If you've worked through the whole thing and
completed the proofs mostly on your own, congratulations! If you want
more practice, there are a lot of directions you could take this:

* Doing trial division all the way up to $n$ is silly; we can stop
  when we get to $\sqrt n$.  I think this would make for a nice
  exercise (in fact, Taneb's version in the Agda stdlib does this).
* Define "$d$ is a proper divisor of $n$" to mean that that $d \mid n$
  and $2 \leq d < n$, then show that the "is a proper divisor of" relation
  is well-founded, and prove `fta` via well-founded induction on that
  relation instead.   This might streamline some parts of the proof;
  I'm not sure.
* The other half of the FTA says that the prime factorization is
  *unique up to permutation*.  To prove this, one has to define
  what it means for one list to be a permutation of another, and show that
  if there are two different prime factorizations then one must be a
  permutation of the other (if $p$ is a prime from the first
  factorization, then it divides the other factorization as well,
  which means it must be equal to one of the primes in the other
  factorization).  I may write up this proof in a follow-up post.
