---
title: 'Competitive Programming in Haskell: stacks, queues, and monoidal sliding windows'
categories: competitive programming,haskell
tags: challenge,Kattis,stack,queue,sliding window,monoid
katex: true
---

Suppose we have a list of items of length $n$, and we want to
consider *windows* (*i.e.* contiguous subsequences) of width $w$
within the list.

^[{-} A list of numbers, with contiguous size-3 windows highlighted]
```{.diagram width=400 caption=""}
dia = vsep 0.2
  [ alignL . hcat $ map drawBox [1,4,2,8,9,4,4,6]
  , vsep 0.2 (map (\d -> hrule 3 # lw thick # lc blue # alignL # translateX d) [0..5])
  ]
  where
    drawBox n = text (show n) # fontSizeL 0.7 <> square 1
```

We can compute the *sum* of each window by brute
force in $O(nw)$ time, by simply generating the list of all the
windows and then summing each.  But, of course, we can do better: keep
track of the sum of the current window; every time we slide the window
one element to the right we can add the new element that enters the
window on the right and subtract the element that falls off the window
to the left. Using this "sliding window" technique, we can compute the
sum of every window in only $O(n)$ total time instead of $O(nw)$.

How about finding the *maximum* of every window? Of course the brute
force $O(nw)$ algorithm still works, but doing it in only $O(n)$ is
considerably trickier!  We can't use the same trick as we did for sums
since there's no way to "subtract" the element falling off the left.
This really comes down to the fact that addition forms a *group*
(*i.e.* a monoid-with-inverses), but `max` does not.  So more
generally, the question is: how can we compute a *monoidal* summary
for every window in only $O(n)$ time?

Today I want to show you how to solve this problem using one of my
favorite competitive programming tricks, which fits beautifully in a
functional context.  Along the way we'll also see how to implement
simple yet efficient functional queues.

Stacks
------

Before we get to queues, we need to take a detour through stacks.
Stacks in Haskell are pretty boring.  We can just use a list, with the
front of the list corresponding to the top of the stack.  However, to
make things more interesting---and because it will come in very handy
later---we're going to implement *monoidally-annotated* stacks.  Every
element on the stack will have a *measure*, which is a value from some
monoid `m`.  We then want to be able to query any stack for the total
of all the measures in $O(1)$.  For example, perhaps we want to always
be able to find the sum or max of all the elements on a stack.

If we wanted to implement stacks annotated by a *group*, we could just
do something like this:

```haskell
data GroupStack g a = GroupStack (a -> g) !g [a]
```

That is, a `GroupStack` stores a *measure function*, which assigns to
each element of type `a` a measure of type `g` (which is intended to
be a `Group`); a value of type `g` representing the sum (via the group
operation) of measures of all elements on the stack; and the actual
stack itself.  To push, we would just compute the measure of the new element
and add it to the cached `g` value; to pop, we subtract the measure of
the element being popped, something like this:

```haskell
push :: a -> GroupStack g a -> GroupStack g a
push a (GroupStack f g as) = GroupStack f (f a <> g) (a:as)

pop :: GroupStack g a -> Maybe (a, GroupStack g a)
pop (GroupStack f g as) = case as of
  [] -> Nothing
  (a:as') -> Just (a, GroupStack f (inv (f a) <> g) as')
```

But this won't work for a monoid, of course. The problem is `pop`, where
we can't just subtract the measure for the element being
popped. Instead, we need to be able to *restore* the measure of a
previous stack.  Hmmm... sounds like we might be able to use... a stack!  We
could just store a stack of measures alongside the stack of elements;
even better is to store a stack of *pairs*.  That is, each element on
the stack is paired with an annotation representing the sum of all the
measures at or below it.  Here, then, is our representation of
monoidally-annotated stacks:

```haskell
{-# LANGUAGE BangPatterns #-}

module Stack where

data Stack m a = Stack (a -> m) !Int [(m, a)]
```

A `Stack m a` stores three things:

1. A measure function of type `a -> m`.^[Incidentally, what if we want
   to be able to specify an arbitrary measure for each element, and
   even give different measures to the same element at different
   times?  Easy: just use `(m,a)` pairs as elements, and use `fst` as
   the measure function.]

2. An `Int` representing the size of the stack.  This is not strictly
   necessary, especially since one could always just use a monoidal
   annotation to keep track of the size; but wanting the size is so
   ubiquitous that it seems convenient to just include it as a special
   case.

3. The aforementioned stack of (annotation, element) pairs.

Note that we cannot write a `Functor` instance for `Stack m`, since
`a` occurs contravariantly in `(a -> m)`.  But this makes sense: if we
change all the `a` values, the cached measures would no longer be valid.

When creating a new, empty stack, we have to specify the measure
function; to get the measure of a stack, we just look up the measure
on top, or return `mempty` for an empty stack.

```haskell
new :: (a -> m) -> Stack m a
new f = Stack f 0 []

size :: Stack m a -> Int
size (Stack _ n _) = n

measure :: Monoid m => Stack m a -> m
measure (Stack _ _ as) = case as of
  [] -> mempty
  (m, _) : _ -> m
```

Now let's implement `push` and `pop`.  Both are relatively
straightforward.

```haskell
push :: Monoid m => a -> Stack m a -> Stack m a
push a s@(Stack f n as) = Stack f (n + 1) ((f a <> measure s, a) : as)

pop :: Stack m a -> Maybe (a, Stack m a)
pop (Stack f n as) = case as of
  [] -> Nothing
  (_, a) : as' -> Just (a, Stack f (n - 1) as')
```

Note that if we care about using *non-commutative* monoids,
in the implementation of `push` we have a choice to make between `f a
<> measure s` and `measure s <> f a`.  The former seems nicer to me,
since it keeps the measures "in the same order" as the list
representing the stack.  For example, if we push a list of elements
onto a stack via `foldr`, using the measure function `(:[])` that injects
each element into the monoid of lists, the resulting `measure` is just
the original list:

```haskell
measure . foldr push (new (:[])) == id
```

And more generally, for any measure function `f`, we have

```haskell
measure . foldr push (new f) == foldMap f
```

Finally, we are going to want a function to *reverse* a stack, which
is a one-liner:

```haskell
reverse :: Monoid m => Stack m a -> Stack m a
reverse (Stack f _ as) = foldl' (flip push) (new f) (map snd as)
```

That is, to reverse a stack, we extract the elements and then use
`foldl'` to push the elements one at a time onto a new stack using the
same measure function.

There is a [bit more code you can find on
GitHub](https://github.com/byorgey/comprog-hs/blob/master/Stack.hs),
such as `Show` and `Eq` instances.

Queues
------

Now that we have monoidally-annotated stacks under our belt, let's
turn to queues.  And here's where my favorite trick is revealed: we
can implement a queue out of two stacks, so that enqueue and dequeue
run in $O(1)$ amortized time; and if we use monoidally-annotated
stacks, we get monoidally-annotated queues for free!

First, some imports.

```haskell
{-# LANGUAGE ImportQualifiedPost #-}

module Queue where

import Data.Bifunctor (second)
import Stack (Stack)
import Stack qualified as Stack
```

A `Queue m a` just consists of two stacks, one for the front and one
for the back.  To create a `new` queue, we just create two new stacks;
to get the `size` of a queue, we just add the sizes of the stacks; to
get the `measure` of a queue, we just combine the measures of the
stacks. Easy peasy.

```haskell
type CommutativeMonoid = Monoid

data Queue m a = Queue {getFront :: Stack m a, getBack :: Stack m a}
  deriving (Show, Eq)

new :: (a -> m) -> Queue m a
new f = Queue (Stack.new f) (Stack.new f)

size :: Queue m a -> Int
size (Queue front back) = Stack.size front + Stack.size back

measure :: CommutativeMonoid m => Queue m a -> m
measure (Queue front back) = Stack.measure front <> Stack.measure back
```

Note the restriction to *commutative* monoids, since the queue
elements are stored in different orders in the front and back stacks.
If we really cared about making this work with non-commutative
monoids, we would have to make two different `push` methods for the
front and back stacks, to combine the measures in opposite orders.
That just doesn't seem worth it.  But if you have a good example
requiring the use of a queue annotated by a non-commutative monoid,
I'd love to hear it!

Now, to `enqueue`, we just push the new element on the back:

```haskell
enqueue :: CommutativeMonoid m => a -> Queue m a -> Queue m a
enqueue a (Queue front back) = Queue front (Stack.push a back)
```

Dequeueing is the magic bit that makes everything work.  If there are
any elements in the front stack, we can just pop from there.
Otherwise, we need to first reverse the back stack into the front
stack.  This means `dequeue` may occasionally take $O(n)$ time, but it's
still $O(1)$ amortized.^[The easiest way to see this is to note that
every element is touched exactly three times: once when it is pushed
on the back; once when it is transferred from the back to the front;
and once when it is popped from the front.  So, overall, we do $O(1)$
work per element.]

```haskell
dequeue :: CommutativeMonoid m => Queue m a -> Maybe (a, Queue m a)
dequeue (Queue front back)
  | Stack.size front == 0 && Stack.size back == 0 = Nothing
  | Stack.size front == 0 = dequeue (Queue (Stack.reverse back) front)
  | otherwise = second (\front' -> Queue front' back) <$> Stack.pop
  front
```

Finally, for convenience, we can make a function `drop1` which just
dequeues an item from the front of a queue and throws it away.

```haskell
drop1 :: CommutativeMonoid m => Queue m a -> Queue m a
drop1 q = case dequeue q of
  Nothing -> q
  Just (_, q') -> q'
```

This "banker's queue" method of building a queue out of two stacks is
discussed in *Purely Functional Data Structures* by Okasaki, though I
don't think he was the first to come up with the idea.  It's also
possible to use some clever tricks to [make both `enqueue` and
`dequeue` take $O(1)$ time in the *worst*
case](https://www.hedonisticlearning.com/posts/global-rebuilding-coroutines-and-defunctionalization.html).
In a future post I'd like to do some benchmarking to compare various
queue implementations (*i.e.* banker's queues, `Data.Sequence`,
circular array queues built on top of `STArray`).  At least
anecdotally, in solving some sliding window problems, banker's queues
seem quite fast so far.

Sliding windows
---------------

I hope you can see how this solves the initial motivating problem: to
find *e.g.* the max of a sliding window, we can just put the elements
in a monoidally-annotated queue, enqueueing and dequeueing one element
every time we slide the window over.^[More generally, of course, it
doesn't even matter if the left and right ends of the window stay
exactly in sync; we can enqueue and dequeue as many times as we want.
] The following `windows` function computes the monoidal sum `foldMap f
window` for each window of width $w$, in only $O(n)$ time
overall.

```haskell
windows :: CommutativeMonoid m => Int -> (a -> m) -> [a] -> [m]
windows w f as = go startQ rest
 where
  (start, rest) = splitAt w as
  startQ = foldl' (flip enqueue) (new f) start

  go q as =
    measure q : case as of
      [] -> []
      a : as -> go (enqueue a (drop1 q)) as
```

"But...maximum and minimum do not form monoids, only semigroups!"
I hear you cry.  Well, we can just adjoin special positive or negative
infinity elements as needed, like so:

```haskell
data Max a = NegInf | Max a deriving (Eq, Ord, Show)

instance Ord a => Semigroup (Max a) where
  NegInf <> a = a
  a <> NegInf = a
  Max a <> Max b = Max (max a b)

instance Ord a => Monoid (Max a) where
  mempty = NegInf

data Min a = Min a | PosInf deriving (Eq, Ord, Show)

instance Ord a => Semigroup (Min a) where
  PosInf <> a = a
  a <> PosInf = a
  Min a <> Min b = Min (min a b)

instance Ord a => Monoid (Min a) where
  mempty = PosInf
```

Now we can write, for example, `windows 3 Max [1,4,2,8,9,4,4,6]` which
yields `[Max 4, Max 8, Max 9, Max 9, Max 9, Max 6]`, the maximums of
each 3-element window.

Challenges
----------

If you'd like to try solving some problems using the techniques from this
blog post, I can recommend the following (generally in order of difficulty):

- [Tired Terry](https://open.kattis.com/problems/tiredterry)
- [Tree Shopping](https://open.kattis.com/problems/treeshopping)
- [Einvígi](https://open.kattis.com/problems/einvigi)
- [Hockey Fans](https://open.kattis.com/problems/hockeyfans)

In a future post I'll walk through my solution to [Hockey
Fans](https://open.kattis.com/problems/hockeyfans). And here's another
couple problems along similar lines; unlike the previous problems I am
not so sure how to solve these in a nice way.  I may write about them
in the future.

- [Martian DNA](https://open.kattis.com/problems/martiandna)
- [Slide Count](https://open.kattis.com/problems/slidecount)
