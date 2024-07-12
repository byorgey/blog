---
title: 'Rivers: eventually constant streams in Haskell'
categories: haskell
tags: Haskell,stream,river,eventually,constant,binary,2-adic,p-adic
katex: true
---

Lately I've been thinking about representing *eventually constant
streams* in Haskell.  An eventually constant stream is an infinite
stream which eventually, after some finite prefix, starts repeating
the same value forever.  For example,

$6, 8, 2, 9, 3, 1, 1, 1, 1, \dots$

There are many things we can do in a
decidable way with eventually constant streams that we can't do with
infinite streams in general---for example, test them for equality.

This is a work in progress.  I only have one specific use case in mind
(infinite-precision two's complement arithmetic, explained at the end
of the post), so I would love to hear of other potential use cases, or
any other feedback.  Depending on the feedback I may eventually turn
this into a package on Hackage.

This blog post is typeset from a literate Haskell file; if you want to
play along you can [download the source from GitHub](https://github.com/byorgey/blog/blob/main/posts/2024/07/16/River.lhs).

The `River` type
----------------

Some preliminaries:

> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE PatternSynonyms #-}
> {-# LANGUAGE ViewPatterns #-}
>
> module River where
>
> import Data.Monoid (All (..), Any (..))
> import Data.Semigroup (Max (..), Min (..))
> import Prelude hiding (all, and, any, drop, foldMap, maximum, minimum, or, repeat, take, zipWith, (!!))
> import Prelude qualified as P

Now let's get to the main definition. A value of type `River a` is
either a constant `C a`, representing an infinite stream of copies of
`a`, or a `Cons` with an `a` and a `River a`.

> data River a = C !a | Cons !a !(River a)
>   deriving Functor

I call this a `River` since "all `River`s flow to the `C`"!

The [strictness annotations](https://tech.fpcomplete.com/haskell/tutorial/all-about-strictness/) on the `a` values just seem like a good
idea in general.  The strictness annotation on the `River a` tail,
however, is more interesting: it's there to rule out infinite streams^[{-} Although the strictness annotation on the `River a` is semantically correct, I could imagine not wanting it there for performance reasons; I'd be happy to hear any feedback on this point.]
constructed using only `Cons`, such as `flipflop = Cons 0 (Cons 1 flipflop)`.  In
other words, the only way to make a non-[bottom](https://en.wikibooks.org/wiki/Haskell/Denotational_semantics) value of type `Stream a` is
to have a finite sequence of `Cons` finally terminated by `C`.

We need to be a bit careful here, since there are multiple ways to
represent streams which are semantically supposed to be the same.  For
example, `Cons 1 (Cons 1 (C 1))` and `C 1` both represent an infinite stream of
all `1`'s.  In general, we have the law

`C a === Cons a (C a)`,

and want to make sure that any functions we write respect this
^[{-} It would be interesting to try implementing rivers as a higher inductive type, say, in [Cubical Agda](https://agda.readthedocs.io/en/v2.6.20240714/language/cubical.html).]
equivalence, *i.e.* do not distinguish between such values.  This is
the reason I did not derive an `Eq` instance; we will have to write
our own.

We can partially solve this problem with a [*bidirectional pattern
synonym*](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pattern_synonyms.html):

> expand :: River a -> River a
> expand (C a) = Cons a (C a)
> expand as = as
>
> infixr 5 :::
> pattern (:::) :: a -> River a -> River a
> pattern (:::) a as <- (expand -> Cons a as)
>   where
>     a ::: as = Cons a as
>
> {-# COMPLETE (:::) #-}

Matching with the pattern `(a ::: as)` uses a [*view pattern*](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/view_patterns.html)
to potentially expand a `C` one step into a `Cons`, so that we can
pretend all `River` values are always constructed with `(:::)`.
In the other direction, `(:::)` merely constructs a `Cons`.

We mark `(:::)` as [`COMPLETE`](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/pragmas.html#complete-pragmas) on its own since it is, in fact,
sufficient to handle every possible input of type `River`.  However,
in order to obtain terminating algorithms we will often include one or
more special cases for `C`.

Normalization by construction?
------------------------------

As an alternative, we could use a variant pattern synonym:

> infixr 5 ::=
> pattern (::=) :: Eq a => a -> River a -> River a
> pattern (::=) a as <- (expand -> Cons a as)
>   where
>     a' ::= C a | a' == a = C a
>     a ::= as = Cons a as
>
> {-# COMPLETE (::=) #-}

As compared to `(:::)`, this has an extra `Eq a` constraint: when we
construct a `River` with `(::=)`, it checks to see whether we are
consing an identical value onto an existing `C a`, and if so, simply
returns the `C a` unchanged.  If we always use `(::=)` instead of
directly using `Cons`, it ensures that `River` values are always
*normalized*---that is, for every eventually constant stream, we
always use the canonical representative where the element immediately
preciding the constant tail is not equal to it.

This, in turn, *technically* makes it impossible to write functions
which do not respect the equivalence `C a === Cons a (C a)`, simply
because they will only ever be given canonical rivers as input.
However, as we will see when we discuss folds, it is still possible to
write "bad" functions, *i.e.* functions that are semantically
questionable as functions on eventually constant streams---it would
just mean we cannot directly observe them behaving badly.

The big downside of using this formulation is that the `Eq` constraint
infects absolutely everything---we even end up with `Eq` constraints
in places where we would not expect them (for example, on `head ::
River a -> a`), because the pattern synonym incurs an `Eq` constraint
anywhere we use it, regardless of whether we are using it to construct
or destruct `River` values.  As you can see from the definition above,
we only do an equality check when using `(::=)` to construct a
`River`, not when using it to pattern-match, but there is no way to
give the pattern synonym different types in the two directions.^[Of course, we could make it a unidirectional pattern synonym and just make a differently named smart constructor, but that seems somewhat ugly, as we would have to remember which to use in which situation.]

So, because this normalizing variant does not really go far enough in
removing our burden of proof, and has some big downsides in the form
of leaking `Eq` constraints everywhere, I have chosen to stick with
the simpler `(:::)` in this post.  But I am still a bit unsure about this
choice; in fact, I went back and forth two times while writing.

We can at least provide a `normalize` function, which we can use when
we want to ensure normalization:

> normalize :: Eq a => River a -> River a
> normalize (C a) = C a
> normalize (a ::= as) = a ::= as

Some standard functions on rivers
----------------------------

With the preliminary definitions out of the way, we can now build up a
library of standard functions and instances for working with `River a`
values.  To start, we can write an `Eq` instance as follows:

> instance Eq a => Eq (River a) where
>   C a == C b = a == b
>   (a ::: as) == (b ::: bs) = a == b && as == bs

Notice that we only need two cases, not four: if we compare two values
whose finite prefixes are different lengths, the shorter one will
automatically expand (via matching on `(:::)`) to the length of the
longer.

We already derived a `Functor` instance; we can also define a "zippy"
`Applicative` instance like so:

> repeat :: a -> River a
> repeat = C
>
> instance Applicative River where
>   pure = repeat
>   C f <*> C x = C (f x)
>   (f ::: fs) <*> (x ::: xs) = f x ::: (fs <*> xs)
>
> zipWith :: (a -> b -> c) -> River a -> River b -> River c
> zipWith = liftA2

We can write safe `head`, `tail`, and index functions:

> head :: River a -> a
> head (a ::: _) = a
>
> tail :: River a -> River a
> tail (_ ::: as) = as
>
> infixl 9 !!
> (!!) :: River a -> Int -> a
> C a !! _ = a
> (a ::: _) !! 0 = a
> (_ ::: as) !! n = as !! (n - 1)

We can also write `take` and `drop` variants.  Note that `take`
returns a finite prefix of a `River`, which is a list, not another
`River`.  The special case for `drop _ (C a)` is not strictly
necessary, but makes it more efficient.

> take :: Int -> River a -> [a]
> take n _ | n <= 0 = []
> take n (a ::: as) = a : take (n - 1) as
>
> drop :: Int -> River a -> River a
> drop n r | n <= 0 = r
> drop _ (C a) = C a
> drop n (_ ::: as) = drop (n - 1) as

There are many other such functions we could implement (*e.g.* `span`,
`dropWhile`, `tails`...); if I eventually put this on Hackage I would
be sure to have a much more thorough selection of functions.  Which
functions would you want to see?

Folds for `River`
-----------------

How do we fold over a `River a`? The `Foldable` type class requires us
to define either `foldMap` or `foldr`; let's think about `foldMap`,
which would have type
```
foldMap :: Monoid m => (a -> m) -> River a -> m
```

However, this doesn't really make sense.  For example, suppose we have
a `River Int`; if we had `foldMap` with the above type, we could use
`foldMap Sum` to turn our `River Int` into a `Sum Int`.  But what is
the sum of an infinite stream of `Int`?  Unless the eventually
repeating part is `C 0`, this is not well-defined.  If we simply write
a function to add up all the `Int` values in a `River`, including
(once) the value contained in the final `C`, this would be a good
example of a semantically "bad" function: it does not respect the law
`C a === a ::: C a`. If we ensure `River` values are always
normalized, we would not be able to directly observe anything amiss,
but the function still seems suspect.

Thinking about the law `C a === a ::: C a` again is the key.
Supposing `foldMap f (C a) = f a` (since it's unclear what else it
could possibly do), applying `foldMap` to both sides of the law we
obtain `f a == f a <> f a`, that is, the combining operation must be
*idempotent*.  This makes sense: with an idempotent operation,
continuing to apply the operation to the infinite constant tail will
not change the answer, so we can simply stop once we reach the `C`.

We can create a subclass of `Semigroup` to represent *idempotent*
semigroups, that is, semigroups for which `a <> a = a`.  There are
several idempotent semigroups in `base`; we list a few below.  Note
that since rivers are never empty, we can get away with just a
semigroup and not a monoid, since we do not need an identity value
onto which to map an empty structure.

> class Semigroup m => Idempotent m
>   -- No methods, since Idempotent represents adding only a law,
>   -- namely, ∀ a. a <> a == a
>
> -- Exercise for the reader: convince yourself that these are all
> -- idempotent
> instance Idempotent All
> instance Idempotent Any
> instance Idempotent Ordering
> instance Ord a => Idempotent (Max a)
> instance Ord a => Idempotent (Min a)

Now, although we cannot make a `Foldable` instance, we can write our own
variant of `foldMap` which requires an idempotent semigroup instead of
a monoid:

> foldMap :: Idempotent m => (a -> m) -> River a -> m
> foldMap f (C a) = f a
> foldMap f (a ::: as) = f a <> foldMap f as
>
> fold :: Idempotent m => River m -> m
> fold = foldMap id

We can then instantiate it at some of the semigroups listed above to
get some useful folds.  These are all guaranteed to terminate and
yield a sensible answer on any `River`.

> and :: River Bool -> Bool
> and = getAll . foldMap All
>
> or :: River Bool -> Bool
> or = getAny . foldMap Any
>
> all :: (a -> Bool) -> River a -> Bool
> all f = and . fmap f
>
> any :: (a -> Bool) -> River a -> Bool
> any f = or . fmap f
>
> maximum :: Ord a => River a -> a
> maximum = getMax . foldMap Max
>
> minimum :: Ord a => River a -> a
> minimum = getMin . foldMap Min
>
> lexicographic :: Ord a => River a -> River a -> Ordering
> lexicographic xs ys = fold $ zipWith compare xs ys

We could make an `instance Ord a => Ord (River a)` with `compare =
lexicographic`; however, in the next section I want to make a
different `Ord` instance for a specific instantiation of `River`.

Application: $2$-adic numbers
-----------------------------

Briefly, here's the particular application I have in mind:
infinite-precision two's complement arithmetic, *i.e.* $2$-adic
numbers.  [Chris Smith also wrote about $2$-adic numbers
recently](https://cdsmithus.medium.com/the-collatz-step-and-2-adic-integers-6f003efaf81c);
however, unlike Chris, I am not interested in $2$-adic numbers in
general, but only specifically those $2$-adic numbers which represent
an embedded copy of $\mathbb{Z}$.  These are precisely the eventually
constant ones: nonnegative integers are represented in binary as
usual, with an infinite tail of $0$ bits, and negative integers are
represented with an infinite tail of $1$ bits.  For example, $-1$ is
represented as an infinite string of all $1$'s.  The amazing thing
about this representation (and the reason it is commonly used in
hardware) is that the usual addition and multiplication algorithms
continue to work without needing special cases to handle negative
integers.  If you've never seen how this works, you should definitely
[read](https://www.cs.cornell.edu/~tomf/notes/cps104/twoscomp.html)
[about it](https://en.wikipedia.org/wiki/Two%27s_complement).

> data Bit = O | I deriving (Eq, Ord, Enum)
>
> type Bits = River Bit

First, some functions to convert to and from integers.  We only need
special cases for $0$ and $-1$, and beyond that it is just the usual
business with `mod` and `div` to peel off one bit at a time, or
multiplying by two and adding to build up one bit at a time. (I am a [big fan of](https://ro-che.info/articles/2014-05-09-clauses) [`LambdaCase`](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/lambda_case.html).)

> toBits :: Integer -> Bits
> toBits = \case
>   0  -> C O
>   -1 -> C I
>   n  -> toEnum (fromIntegral (n `mod` 2)) ::: toBits (n `div` 2)
>
> fromBits :: Bits -> Integer
> fromBits = \case
>   C O -> 0
>   C I -> -1
>   b ::: bs -> 2 * fromBits bs + fromIntegral (fromEnum b)

For testing, we can also make a `Show` instance.  When it comes to
showing the infinite constant tail, I chose to repeat the bit 3 times
and then show an ellipsis; this is not really necessary but somehow
helps my brain more easily see whether it is an infinite tail of zeros
or ones.

> instance Show Bits where
>   show = reverse . go
>    where
>     go (C b) = replicate 3 (showBit b) ++ "..."
>     go (b ::: bs) = showBit b : go bs
>
>     showBit = ("01" P.!!) . fromEnum

Let's try it out:

    ghci> toBits 26
    ...00011010
    ghci> toBits (-30)
    ...11100010
    ghci> fromBits (toBits (-30))
    -30
    ghci> quickCheck $ \x -> fromBits (toBits x) == x
    +++ OK, passed 100 tests.

Arithmetic on $2$-adic numbers
------------------------------

Let's implement some arithmetic.  First, incrementing.  It is standard
except for a special case for `C I` (without which, incrementing `C I`
would diverge).  Notice that we use `(::=)` instead of `(:::)`, which
ensures our `Bits` values remain normalized.

> inc :: Bits -> Bits
> inc = \case
>   C I      -> C O
>   O ::= bs -> I ::= bs
>   I ::= bs -> O ::= inc bs

`dec` is similar, just the opposite:

> dec :: Bits -> Bits
> dec = \case
>   C O      -> C I
>   I ::= bs -> O ::= bs
>   O ::= bs -> I ::= dec bs

Then we can write `inv` to invert all bits, and `neg` as the
composition of `inc` and `inv`.

> inv :: Bits -> Bits
> inv = fmap $ \case { O -> I; I -> O }
>
> neg :: Bits -> Bits
> neg = inc . inv

Trying it out:

    λ> toBits 3
    ...00011
    λ> neg it
    ...11101
    λ> inc it
    ...1110
    λ> inc it
    ...111
    λ> inc it
    ...000
    λ> inc it
    ...0001
    λ> dec it
    ...000
    λ> dec it
    ...111

Finally, addition, multiplication, and `Ord` and `Num` instances:

> add :: Bits -> Bits -> Bits
> add = \cases
>   (C O)      y          -> y
>   x          (C O)      -> x
>   (C I)      (C I)      -> O ::= C I
>   (I ::= xs) (I ::= ys) -> O ::= inc (add xs ys)
>   (x ::= xs) (y ::= ys) -> (x .|. y) ::= add xs ys
>  where
>   I .|. _ = I
>   _ .|. y = y
>
> mul :: Bits -> Bits -> Bits
> mul = \cases
>   (C O)      _     -> C O
>   _          (C O) -> C O
>   (C I)      y     -> neg y
>   x          (C I) -> neg x
>   (O ::= xs) ys    ->         O ::= mul xs ys
>   (I ::= xs) ys    -> add ys (O ::= mul xs ys)
>
> instance Ord Bits where
>   -- It's a bit mind-boggling that this works
>   compare (C x) (C y) = compare y x
>   compare (x ::= xs) (y ::= ys) = compare xs ys <> compare x y
>
> instance Num Bits where
>   fromInteger = toBits
>   negate = neg
>   (+) = add
>   (*) = mul
>   abs = toBits . abs . fromBits
>   signum = toBits . signum . fromBits

    λ> quickCheck $ withMaxSuccess 1000 $ \x y -> fromBits (mul (toBits x) (toBits y)) == x * y
    +++ OK, passed 1000 tests.
    λ> quickCheck $ \x y -> compare (toBits x) (toBits y) == compare x y
    +++ OK, passed 100 tests.

Just for fun, let's implement the Collatz map:

> collatz :: Bits -> Bits
> collatz (O ::= bs) = bs
> collatz bs@(I ::= _) = 3*bs + 1

    λ> P.take 20 $ map fromBits (iterate collatz (toBits (-13)))
    [-13,-38,-19,-56,-28,-14,-7,-20,-10,-5,-14,-7,-20,-10,-5,-14,-7,-20,-10,-5]
    λ> P.take 20 $ map fromBits (iterate collatz (toBits 7))
    [7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1,4,2,1]

Questions / future work
-----------------------

- Is `(:::)` or `(::=)` the better default?  It's tempting to just say
  "provide both and let the user decide".  I don't disagree with that;
  however, the question is which one we use to implement various basic
  functions such as `map`/`fmap`.  For example, if we use `(:::)`, we
  can make a `Functor` instance, but values may not be normalized
  after mapping.

- Can we generalize from eventually constant to eventually *periodic*?
  That is, instead of repeating the same value forever, we cycle
  through a repeating period of some finite length.  I think this
  is possible, but it would make the implementation more
  complex, and I don't know the right way to generalize `foldMap`. (We
  could insist that it only works for *commutative* idempotent
  semigroups, but in that case what's the point of having a *sequence*
  of values rather than just a set?)

Happy to hear any comments or suggestions!
