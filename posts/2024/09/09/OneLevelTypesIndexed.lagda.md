---
title: Decidable equality for indexed data types
categories: agda
tags: agda,equality,indexed
katex: true
---

Recently, as part of a larger project, I wanted to define decidable
equality for an indexed data type in Agda.  I struggled quite a bit to
figure out the right way to encode it to make Agda happy, and wasn't
able to find much help online, so I'm recording the results here.

The **tl;dr** is to use mutual recursion to define the indexed data
type along with a sigma type that hides the index, and to use the
sigma type in any recursive positions where we don't care about the
index!  Read on for more motivation and details (and wrong turns I
took along the way).

This post is literate Agda; you can [download it here](https://github.com/byorgey/blog/blob/main/posts/2024/09/09/OneLevelTypesIndexed.lagda.md) if you want to play along.  I tested everything here with Agda version 2.6.4.3 and version 2.0 of the standard library.

Background
----------

First, some imports and a module declaration.  Note that the entire
development is parameterized by some abstract set `B` of base types,
which must have decidable equality.

```agda
open import Data.Product using (Σ ; _×_ ; _,_ ; -,_ ; proj₁ ; proj₂)
open import Data.Product.Properties using (≡-dec)
open import Function using (_∘_)
open import Relation.Binary using (DecidableEquality)
open import Relation.Binary.PropositionalEquality using (_≡_ ; refl)
open import Relation.Nullary.Decidable using (yes; no; Dec)

module OneLevelTypesIndexed (B : Set) (≟B : DecidableEquality B) where
```

We'll work with a simple type system containing base types, function
types, and some distinguished type constructor □.  So far, this is
just to give some context; it is not the final version of the code we
will end up with, so we stick it in a local module so it won't end up
in the top-level namespace.

```agda
module Unindexed where
  data Ty : Set where
    base : B → Ty
    _⇒_ : Ty → Ty → Ty
    □_ : Ty → Ty
```

For example, if $X$ and $Y$ are base types, then we could write down a
type like $\square ((\square \square X \to Y) \to \square Y)$:

```agda
  infixr 2 _⇒_
  infix 30 □_

  postulate
    BX BY : B

  X : Ty
  X = base BX
  Y : Ty
  Y = base BY

  example : Ty
  example = □ ((□ □ X ⇒ Y) ⇒ □ Y)
```

However, for reasons that would take us too far afield in this blog
post, I *don't* want to allow immediately nested boxes, like $\square
\square X$.  We can still have multiple boxes in a type, and even
boxes nested inside of other boxes, as long as there is at least one
arrow in between.  In other words, I only want to rule out boxes
immediately applied to another type with an outermost box.  So we
don't want to allow the example type given above (since it contains
$\square \square X$), but, for example, $\square ((\square X \to Y)
\to \square Y)$ would be OK.

Encoding invariants
-------------------

How can we encode this invariant so it holds by construction?  One way
would be to have two mutually recursive data types, like so:

```agda
module Mutual where
  data Ty : Set
  data UTy : Set

  data Ty where
    □_ : UTy → Ty
    ∙_ : UTy → Ty

  data UTy where
    base : B → UTy
    _⇒_ : Ty → Ty → UTy
```

`UTy` consists of types which have no top-level box; the constructors
of `Ty` just inject `UTy` into `Ty` by adding either one or zero
boxes.  This works, and defining decidable equality for `Ty` and `UTy`
is relatively straightforward (again by mutual recursion).  However,
it seemed to me that having to deal with `Ty` and `UTy` everywhere
through the rest of the development was probably going to be super
annoying.

The other option would be to index `Ty` by values indicating whether a
type has zero or one top-level boxes; then we can use the indices to
enforce the appropriate rules.  First, we define a data type `Boxity`
to act as the index for `Ty`, and show that it has decidable equality:

```agda
data Boxity : Set where
  [0] : Boxity
  [1] : Boxity

Boxity-≟ : DecidableEquality Boxity
Boxity-≟ [0] [0] = yes refl
Boxity-≟ [0] [1] = no λ ()
Boxity-≟ [1] [0] = no λ ()
Boxity-≟ [1] [1] = yes refl
```

My first attempt to write down a version of `Ty` indexed by `Boxity`
looked like this:

```agda
module IndexedTry1 where
  data Ty : Boxity → Set where
    base : B → Ty [0]
    _⇒_ : {b₁ b₂ : Boxity} → Ty b₁ → Ty b₂ → Ty [0]
    □_ : Ty [0] → Ty [1]
```

`base` always introduces a type with no top-level box; the `□`
constructor requires a type with no top-level box, and produces a type
with one (this is what ensures we cannot nest boxes); and the arrow
constructor does not care how many boxes its arguments have, but
constructs a type with no top-level box.

This is logically correct, but I found it very difficult to work with.
The sticking point for me was injectivity of the arrow constructor.
When defining decidable equality we need to prove lemmas that each of
the constructors are injective, but I was not even able to write down
the *type* of injectivity for `_⇒_`.  We would want something like this:

```haskell
⇒-inj :
  {bσ₁ bσ₂ bτ₁ bτ₂ : Boxity}
  {σ₁ : Ty bσ₁} {σ₂ : Ty bσ₂} {τ₁ : Ty bτ₁} {τ₂ : Ty bτ₂} →
  (σ₁ ⇒ σ₂) ≡ (τ₁ ⇒ τ₂) →
  (σ₁ ≡ τ₁) × (σ₂ ≡ τ₂)
```

but this does not even typecheck!  The problem is that, for example,
`σ₁` and `τ₁` have different types, so the equality proposition `σ₁ ≡
τ₁` is not well-typed.

At this point I tried turning to [heterogeneous
equality](https://agda.github.io/agda-stdlib/experimental/Relation.Binary.HeterogeneousEquality.html),
but it didn't seem to help.  I won't record here all the things I
tried, but the same issues seemed to persist, just pushed around to
different places (for example, I was not able to pattern-match on
witnesses of heterogeneous equality because of types that didn't
match).

Sigma types to the rescue
-------------------------

At ICFP last week I asked [Jesper Cockx](https://jesper.sikanda.be/)
for advice,^[which felt a bit like asking Rory McIlroy to give some
tips on your mini-golf game] and he suggested trying to prove
decidable equality for the sigma type pairing an index with a type
having that index, like this:

```agda
  ΣTy : Set
  ΣTy = Σ Boxity Ty
```

This turned out to be the key idea, but it still took me a long time
to figure out the right way to make it work.  Given the above
definitions, if we go ahead and try to define decidable equality for
`ΣTy`, injectivity of the arrow constructor is still a problem.

After days of banging my head against this off and on, I finally
realized that the way to solve this is to define `Ty` and `ΣTy` by
mutual recursion: the arrow constructor should just take two `ΣTy`
arguments!  This perfectly captures the idea that we *don't care*
about the indices of the arrow constructor's argument types, so we
hide them by bundling them up in a sigma type.

```agda
ΣTy : Set
data Ty : Boxity → Set

ΣTy = Σ Boxity Ty

data Ty where
  □_ : Ty [0] → Ty [1]
  base : B → Ty [0]
  _⇒_ : ΣTy → ΣTy → Ty [0]

infixr 2 _⇒_
infix 30 □_
```

Now we're cooking!  We now make quick work of the required injectivity
lemmas, which all go through trivially by matching on `refl`:

```agda

□-inj : {τ₁ τ₂ : Ty [0]} → (□ τ₁ ≡ □ τ₂) → (τ₁ ≡ τ₂)
□-inj refl = refl

base-inj : {b₁ b₂ : B} → base b₁ ≡ base b₂ → b₁ ≡ b₂
base-inj refl = refl

⇒-inj : {σ₁ σ₂ τ₁ τ₂ : ΣTy} → (σ₁ ⇒ σ₂) ≡ (τ₁ ⇒ τ₂) → (σ₁ ≡ τ₁) × (σ₂ ≡ τ₂)
⇒-inj refl = refl , refl
```

Notice how the type of `⇒-inj` is now perfectly fine: we just have a
bunch of `ΣTy` values that hide their indices, so we can talk about
propositional equality between them with no trouble.

Finally, we can define decidable equality for `Ty` and `ΣTy` by mutual
recursion.

```agda
ΣTy-≟ : DecidableEquality ΣTy

{-# TERMINATING #-}
Ty-≟ : ∀ {b} → DecidableEquality (Ty b)
```

Sadly, I had to reassure Agda that the definition of `Ty-≟` is terminating---more on this later.

To define `ΣTy-≟` we can just use a lemma from
`Data.Product.Properties` which derives decidable equality for a sigma
type from decidable equality for both components.

```agda
ΣTy-≟ = ≡-dec Boxity-≟ Ty-≟
```

The only thing left is to define decidable equality for any two values
of type `Ty b` (given a specific boxity `b`), making use of our
injectivity lemmas; now that we have the right definitions, this falls
out straightforwardly.

```agda
Ty-≟ (□ σ) (□ τ) with Ty-≟ σ τ
... | no σ≢τ = no (σ≢τ ∘ □-inj)
... | yes refl = yes refl
Ty-≟ (base x) (base y) with ≟B x y
... | no x≢y = no (x≢y ∘ base-inj)
... | yes refl = yes refl
Ty-≟ (σ₁ ⇒ σ₂) (τ₁ ⇒ τ₂) with ΣTy-≟ σ₁ τ₁ | ΣTy-≟ σ₂ τ₂
... | no σ₁≢τ₁ | _ = no (σ₁≢τ₁ ∘ proj₁ ∘ ⇒-inj)
... | yes _ | no σ₂≢τ₂ = no (σ₂≢τ₂ ∘ proj₂ ∘ ⇒-inj)
... | yes refl | yes refl = yes refl
Ty-≟ (base _) (_ ⇒ _) = no λ ()
Ty-≟ (_ ⇒ _) (base _) = no λ ()
```

Final thoughts
--------------

First, the one remaining infelicity is that Agda could not tell that
`Ty-≟` is terminating.  I am not entirely sure why, but I think it may
be that the way the recursion works is just too convoluted for it to
analyze properly: `Ty-≟` calls `ΣTy-≟` on structural subterms of its
inputs, but then `ΣTy-≟` works by providing `Ty-≟` *as a higher-order
parameter* to `≡-dec`.  If you look at the definition of `≡-dec`, all
it does is call its function parameters on structural subterms of its
input, so everything should be nicely terminating, but I guess I am
not surprised that Agda is not able to figure this out.  If anyone has
suggestions on how to make this pass the termination checker without
using a `TERMINATING` pragma, I would love to hear it!

As a final aside, I note that converting back and forth between `Ty`
(with `ΣTy` arguments to the arrow constructor) and `IndexedTry1.Ty`
(with expanded-out `Boxity` and `Ty` arguments to arrow) is trivial:

```agda
Ty→Ty1 : {b : Boxity} → Ty b → IndexedTry1.Ty b
Ty→Ty1 (□ σ) = IndexedTry1.□ (Ty→Ty1 σ)
Ty→Ty1 (base x) = IndexedTry1.base x
Ty→Ty1 ((b₁ , σ₁) ⇒ (b₂ , σ₂)) = (Ty→Ty1 σ₁) IndexedTry1.⇒ (Ty→Ty1 σ₂)

Ty1→Ty : {b : Boxity} → IndexedTry1.Ty b → Ty b
Ty1→Ty (IndexedTry1.base x) = base x
Ty1→Ty (σ₁ IndexedTry1.⇒ σ₂) = -, (Ty1→Ty σ₁) ⇒ -, (Ty1→Ty σ₂)
Ty1→Ty (IndexedTry1.□ σ) = □ (Ty1→Ty σ)
```

I expect it is also trivial to prove this is an isomorphism, though
I'm not particularly motivated to do it.  The point is that, as anyone
who has spent any time proving things with proof assistants knows, two
types can be completely isomorphic, and yet one can be vastly easier
to work with than the other in certain contexts.  Often when I'm
trying to prove something in Agda it feels like at least half the
battle is just coming up with the right representation that makes the
proofs go through easily.
