---
title: Decidable equality for indexed data types, take 2
categories: agda
tags: agda,equality,path,dependent,indexed
katex: true
---

In a [post from a year
ago](https://byorgey.github.io/blog/posts/2024/09/09/OneLevelTypesIndexed.lagda.html),
I explored how to prove decidable equality in Agda of a particular
indexed data type.  Recently, I discovered a different way to
accomplish the same thing, without resorting to embedded sigma types.

This post is literate Agda; you can [download it
here](https://github.com/byorgey/blog/blob/main/posts/2025/08/22/OneLevelTypesIndexed2.lagda.md)
if you want to play along.  I tested everything here with Agda version
2.6.4.3 and version 2.0 of the standard library. (I assume it would
also work with more recent versions, but haven't tested it.)

Background
----------

*This section is repeated from my [previous
post](https://byorgey.github.io/blog/posts/2024/09/09/OneLevelTypesIndexed.lagda.html),
which I assume no one remembers.*

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

module OneLevelTypesIndexed2 (B : Set) (≟B : DecidableEquality B) where
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

Two encodings
-------------

In my [previous blog
post](https://byorgey.github.io/blog/posts/2024/09/09/OneLevelTypesIndexed.lagda.html),
I ended up with the following encoding of types indexed by a `Boxity`,
which records the number of top-level boxes.  Since the boxity of the
arguments to an arrow type do not matter, we make them sigma types
that package up a boxity with a type having that boxity.  I was then
able to define decidable equality for `ΣTy` and `Ty` by mutual
recursion.

```agda
data Boxity : Set where
  ₀ : Boxity
  ₁ : Boxity

variable b b₁ b₂ b₃ b₄ : Boxity

module WithSigma where
  ΣTy : Set
  data Ty : Boxity → Set

  ΣTy = Σ Boxity Ty

  data Ty where
    □_ : Ty ₀ → Ty ₁
    base : B → Ty ₀
    _⇒_ : ΣTy → ΣTy → Ty ₀

```

The problem is that working with this definition of `Ty` is really
annoying!  Every time we construct or pattern-match on an arrow type,
we have to package up each argument type into a dependent pair with
its `Boxity`; this introduces syntactic clutter, and in many cases we
know exactly what the `Boxity` has to be, so it's not even
informative.  The version we really want looks more like this:

```agda
data Ty : Boxity → Set where
  base : B → Ty ₀
  _⇒_ : {b₁ b₂ : Boxity} → Ty b₁ → Ty b₂ → Ty ₀
  □_ : Ty ₀ → Ty ₁

infixr 2 _⇒_
infix 30 □_
```

In this version, the boxities of the arguments to the arrow
constructor are just implicit parameters of the arrow constructor
itself.  Previously, I was unable to get decidable equality to go
through for this version... but just the other day, I finally realized
how to make it work!

Path-dependent equality
-----------------------

The key trick that makes everything work is to define a
*path-dependent equality* type.  I [learned this from Martín
Escardó](https://martinescardo.github.io/dependent-equality-lecture/DependentEquality.html).
The idea is that we can express equality between two indexed things
with different indices, as long as we also have an equality between
the indices.

```agda
_≡⟦_⟧_ : {A : Set} {B : A → Set} {a₀ a₁ : A} → B a₀ → a₀ ≡ a₁ → B a₁ → Set
b₀ ≡⟦ refl ⟧ b₁   =   b₀ ≡ b₁
```

That's exactly what we need here: the ability to express
equality between `Ty` values, which may be indexed by different
boxities---as long as we know that the boxities are equal.

Decidable equality for `Ty`
---------------------------

We can now use this to directly encode decidable equality for `Ty`.
First, we can easily define decidable equality for `Boxity`.

```agda

Boxity-≟ : DecidableEquality Boxity
Boxity-≟ ₀ ₀ = yes refl
Boxity-≟ ₀ ₁ = no λ ()
Boxity-≟ ₁ ₀ = no λ ()
Boxity-≟ ₁ ₁ = yes refl
```

Here is the type of the decision procedure: given two `Ty` values
which may have *different* boxities, we decide whether or not we can
produce a witness to their equality.  Such a witness consists of a
*pair* of (1) a proof that the boxities are equal, and (2) a proof
that the types are equal, depending on (1).^[We would really like to
write this as `Σ (b₁ ≡ b₂) λ p → σ ≡⟦ p ⟧ τ`, but for some reason Agda
requires us to fill in some extra implicit arguments before it is
happy that everything is unambiguous, [requiring some ugly syntax](https://github.com/agda/agda/issues/2264).]

```agda
Ty-≟′ : (σ : Ty b₁) → (τ : Ty b₂) → Dec (Σ (b₁ ≡ b₂) λ p → _≡⟦_⟧_ {_} {Ty} σ p τ)
```

Before showing the definition of `Ty-≟′`, let's see that we can use it
to easily define both a boxity-homogeneous version of decidable
equality for `Ty`, as well as decidable equality for `Σ Boxity Ty`:

```agda
Ty-≟ : DecidableEquality (Ty b)
Ty-≟ {b} σ τ with Ty-≟′ σ τ
... | no σ≢τ = no (λ σ≡τ → σ≢τ ( refl , σ≡τ))
... | yes (refl , σ≡τ) = yes σ≡τ

ΣTy-≟ : DecidableEquality (Σ Boxity Ty)
ΣTy-≟ (_ , σ) (_ , τ) with Ty-≟′ σ τ
... | no σ≢τ = no λ { refl → σ≢τ (refl , refl) }
... | yes (refl , refl) = yes refl
```

A lot of pattern matching on `refl` and everything falls out quite easily.

And now the definition of `Ty-≟′`.  It looks complicated, but it is
actually not very difficult.  The most interesting case is when
comparing two arrow types for equality: we must first compare the
boxities of the arguments, then consider the arguments themselves once
we know the boxities are equal.

```agda
Ty-≟′ (□ σ) (□ τ) with Ty-≟′ σ τ
... | yes (refl , refl) = yes (refl , refl)
... | no σ≢τ = no λ { (refl , refl) → σ≢τ (refl , refl) }
Ty-≟′ (base S) (base T) with ≟B S T
... | yes refl = yes (refl , refl)
... | no S≢T = no λ { (refl , refl) → S≢T refl }
Ty-≟′ (_⇒_ {b₁} {b₂} σ₁ σ₂) (_⇒_ {b₃} {b₄} τ₁ τ₂) with Boxity-≟ b₁ b₃ | Boxity-≟ b₂ b₄ | Ty-≟′ σ₁ τ₁ | Ty-≟′ σ₂ τ₂
... | no b₁≢b₃ | _ | _ | _ = no λ { (refl , refl) → b₁≢b₃ refl }
... | yes _ | no b₂≢b₄ | _ | _ = no λ { (refl , refl) → b₂≢b₄ refl }
... | yes _ | yes _ | no σ₁≢τ₁ | _ = no λ { (refl , refl) → σ₁≢τ₁ (refl , refl) }
... | yes _ | yes _ | yes _ | no σ₂≢τ₂ = no λ { (refl , refl) → σ₂≢τ₂ (refl , refl) }
... | yes _ | yes _ | yes (refl , refl) | yes (refl , refl) = yes (refl , refl)
Ty-≟′ (□ _) (base _) = no λ ()
Ty-≟′ (□ _) (_ ⇒ _) = no λ ()
Ty-≟′ (base _) (□ _) = no λ ()
Ty-≟′ (base _) (_ ⇒ _) = no λ { (refl , ()) }
Ty-≟′ (_ ⇒ _) (□ _) = no λ ()
Ty-≟′ (_ ⇒ _) (base _) = no λ { (refl , ()) }
```


