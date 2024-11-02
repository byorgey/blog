---
title: 'Competitive Programming in Haskell: Union-Find'
categories: competitive programming,haskell
tags: challenge,Kattis,union-find
katex: true
---

Union-find
----------

A [*union-find* data
structure](https://en.wikipedia.org/wiki/Disjoint-set_data_structure)
(also known as a *disjoint set* data structure) keeps track of a
*collection of disjoint sets*, typically with elements drawn from
$\{0, \dots, n-1\}$.  For example, we might have the sets

$\{1,3\}, \{0, 4, 2\}, \{5, 6, 7\}$

A union-find structure must support three basic operations:

\newcommand{\create}{\mathit{create}}
\newcommand{\find}{\mathit{find}}
\newcommand{\union}{\mathit{union}}

- We can $\create$ a union-find structure with $n$ singleton sets
  $\{0\}$ through $\{n-1\}$.  (Alternatively, we could support two
  operations: creating an empty union-find structure, and adding a new
  singleton set; occasionally this more fine-grained approach is
  useful, but we will stick with the simpler $\create$ API for now.)

- We can $\find$ a given $x \in \{0, \dots, n-1\}$, returning some sort
  of "name" for the set $x$ is in.  It doesn't matter what these names
  are; the only thing that matters is that for any $x$ and $y$,
  $\find(x) = \find(y)$ if and only if $x$ and $y$ are in the same
  set.  The most important application of $\find$ is therefore to check
  whether two given elements are in the same set or not.

- We can $\union$ two elements, so the sets that contain them become
  one set.  For example, if we $\union(2,5)$ then we would have

    $\{1,3\}, \{0, 4, 2, 5, 6, 7\}$

Note that $\union$ is a one-way operation: once two sets have been
unioned together, there's no way to split them apart again.  (If both
merging and splitting are required, one can use a [link/cut
tree](https://en.wikipedia.org/wiki/Link/cut_tree), which is very cool---and possibly something I will write
about in the future---but much more complex.)  However, these three
operations are enough for union-find structures to have a large number
of interesting applications!

In addition, we can annotate each set with a value taken from some
commutative semigroup.  When creating a new union-find structure, we
must specify the starting value for each singleton set; when unioning
two sets, we combine their annotations via the semigroup operation.

- For example, we could annotate each set with its *size*; singleton
  sets always start out with size 1, and every time we union two sets
  we add their sizes.
- We could also annotate each set with the sum, product, maximum, or
  minumum of all its elements.
- Of course there are many more exotic examples as well.

We typically use a commutative semigroup, as in the examples above;
this guarantees that a given set always has a single well-defined
annotation value, regardless of the sequence of union-find operations
that were used to create it.  However, we can actually use any binary
operation at all (*i.e.* any *magma*), in which case the annotations
on a set may reflect the precise tree of calls to $\union$ that were
used to construct it; this can occasionally be useful.

- For example, we could annotate each set with a list of values, and
  combine annotations using list concatenation; the order of elements
  in the list associated to a given set will depend on the order of
  arguments to $\union$.

- We could also annotate each set with a binary tree storing values at
  the leaves. Each singleton set is annotated with a single leaf; to
  combine two trees we create a new branch node with the two trees as
  its children.  Then each set ends up annotated with the precise tree
  of calls to $\union$ that were used to create it.

Implementing union-find
-----------------------

My implementation is based on [one by Kwang Yul
Seo](https://kseo.github.io/posts/2014-01-30-implementing-union-find-in-haskell.html),
but I have modified it quite a bit.  The code is [also available in my
`comprog-hs`
repository](https://github.com/byorgey/comprog-hs/blob/master/UnionFind.hs). This
blog post is not intended to be a comprehensive union-find tutorial,
but I will explain some things as we go.

> {-# LANGUAGE RecordWildCards #-}
>
> module UnionFind where
>
> import Control.Monad (when)
> import Control.Monad.ST
> import Data.Array.ST

Let's start with the definition of the `UnionFind` type itself.
`UnionFind` has two type parameters: `s` is a phantom type parameter
used to limit the scope to a given `ST` computation; `m` is the type
of the arbitrary annotations.  Note that the elements are also
sometimes called "nodes", since, as we will see, they are organized
into trees.

> type Node = Int
> data UnionFind s m = UnionFind {

The basic idea is to maintain three mappings:

  - First, each element is mapped to a *parent* (another element).
    There are no cycles, except that some elements can be their own
    parent.  This means that the elements form a *forest* of rooted
    trees, with the self-parenting elements as roots.  We
    store the parent mapping as an `STUArray` ([see here](https://byorgey.github.io/blog/posts/2021/11/17/competitive-programming-in-haskell-bfs-part-4-implementation-via-stuarray.html) for another post where we used `STUArray`) for
    efficiency.

>   parent :: !(STUArray s Node Node),

  - Each element is also mapped to a *size*.  We maintain the
    invariant that for any element which is a root (*i.e.* any element
    which is its own parent), we store the size of the tree rooted at
    that element.  The size associated to other, non-root elements
    does not matter.

    (Many implementations store the *height* of each tree instead of
    the size, but it does not make much practical difference, and the
    size seems more generally useful.)

>   sz :: !(STUArray s Node Int),

  - Finally, we map each element to a custom annotation value; again,
    we only care about the annotation values for root nodes.

>   ann :: !(STArray s Node m) }

To $\create$ a new union-find structure, we need a size and a
function mapping each element to an initial annotation value.  Every
element starts as its own parent, with a size of 1.  For convenience,
we can also make a variant of `createWith` that gives every element
the same constant annotation value.

> createWith :: Int -> (Node -> m) -> ST s (UnionFind s m)
> createWith n m =
>   UnionFind
>     <$> newListArray (0, n - 1) [0 .. n - 1]    -- Every node is its own parent
>     <*> newArray (0, n - 1) 1                   -- Every node has size 1
>     <*> newListArray (0, n - 1) (map m [0 .. n - 1])
>
> create :: Int -> m -> ST s (UnionFind s m)
> create n m = createWith n (const m)

To perform a $\find$ operation, we keep following *parent*
references up the tree until reaching a root.  We can also do a cool
optimization known as *path compression*: after finding a
root, we can directly update the parent of every node along the path
we just traversed to be the root.  This means $\find$ can be very
efficient, since it tends to create trees that are extremely wide and
shallow.

> find :: UnionFind s m -> Node -> ST s Node
> find uf@(UnionFind {..}) x = do
>   p <- readArray parent x
>   if p /= x
>     then do
>       r <- find uf p
>       writeArray parent x r
>       pure r
>     else pure x
>
> connected :: UnionFind s m -> Node -> Node -> ST s Bool
> connected uf x y = (==) <$> find uf x <*> find uf y

Finally, to implement $\union$, we find the roots of the given nodes;
if they are not the same we make the root with the smaller tree the
child of the other root, combining sizes and annotations as
appropriate.

> union :: Semigroup m => UnionFind s m -> Node -> Node -> ST s ()
> union uf@(UnionFind {..}) x y = do
>   x <- find uf x
>   y <- find uf y
>   when (x /= y) $ do
>     sx <- readArray sz x
>     sy <- readArray sz y
>     mx <- readArray ann x
>     my <- readArray ann y
>     if sx < sy
>       then do
>         writeArray parent x y
>         writeArray sz y (sx + sy)
>         writeArray ann y (mx <> my)
>       else do
>         writeArray parent y x
>         writeArray sz x (sx + sy)
>         writeArray ann x (mx <> my)

Note the trick of writing `x <- find uf x`: this looks kind of like an
imperative statement that updates the value of a mutable variable `x`,
but really it just makes a new variable `x` which shadows the old
one.

Finally, a few utility functions.  First, one to get the size of
the set containing a given node:

> size :: UnionFind s m -> Node -> ST s Int
> size uf@(UnionFind {..}) x = do
>   x <- find uf x
>   readArray sz x

Also, we can provide functions to update and fetch the custom
annotation value associated to the set containing a given node.

> updateAnn :: Semigroup m => UnionFind s m -> Node -> m -> ST s ()
> updateAnn uf@(UnionFind {..}) x m = do
>   x <- find uf x
>   old <- readArray ann x
>   writeArray ann x (old <> m)
>   -- We could use modifyArray above, but the version of the standard library
>   -- installed on Kattis doesn't have it
>
> getAnn :: UnionFind s m -> Node -> ST s m
> getAnn uf@(UnionFind {..}) x = do
>   x <- find uf x
>   readArray ann x

Challenge
---------

Here are a couple of problems I challenge you to solve for next time:

- [Duck Journey](https://open.kattis.com/problems/andvag)
- [Inventing Test Data](https://open.kattis.com/problems/inventing)
