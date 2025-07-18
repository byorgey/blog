---
title: 'Competitive programming in Haskell: range queries, classified'
categories: Haskell,competitive programming
katex: true
tags: semigroup,monoid,range,query
---

Static range queries
--------------------

Suppose we have a sequence of values, which is *static* in the sense
that the values in the sequence will never change, and we want to
perform *range queries*, that is, for various ranges we want to
compute the total of all consecutive values in the range, according to
some binary combining operation.  For example, we might want to
compute the maximum, sum, or product of all the consecutive values in
a certain subrange.  We have various options depending on the kind of
ranges we want and the algebraic properties of the operation.

- If we want ranges corresponding to a *sliding window*, we can use
  [an amortized queue
  structure](https://byorgey.github.io/blog/posts/2024/11/27/stacks-queues.html)
  to find the total of each range in $O(1)$, for an arbitrary
  monoid.
  <!-- ^[If we have a group, then no special data structure is -->
  <!-- needed: just keep track of the value of the current window, and add -->
  <!-- and subtract values as they enter and leave the window, -->
  <!-- respectively.] -->

- If we want arbitrary ranges but the operation is a *group*, the
  solution is relatively straightforward: we can precompute all
  *[prefix sums](https://byorgey.github.io/blog/posts/2025/06/27/prefix-sums.html)*, and subtract to find the result for an arbitrary
  range in $O(1)$.

- If the operation is an *idempotent semigroup* (that is, it has the
  property that $x \diamond x = x$ for all $x$), we can use a *[sparse
  table](https://byorgey.github.io/blog/posts/2025/07/18/sparse-table.html)*, which takes $O(n \lg n)$ time and space for precomputation,
  and then allows us to answer arbitrary range queries in $O(1)$.

- If the operation is an arbitrary monoid, we can use a *[sqrt tree](https://cp-algorithms.com/data_structures/sqrt-tree.html)*,
  which uses $O(n \lg \lg n)$ precomputed time and space, and allows
  answering arbitrary range queries in $O(\lg \lg n)$.  I will write
  about this in a future post.

Dynamic range queries
---------------------

What if we want *dynamic* range queries, that is, we want to be able
to interleave range queries with arbitrary updates to the values of
the sequence?

- If the operation is an arbitrary monoid, we can use a segment
  tree.
- If the operation is a group, we can use a [Fenwick tree](https://cp-algorithms.com/data_structures/fenwick.html).

I published [a paper about Fenwick
  trees](https://byorgey.github.io/blog/posts/2025/01/23/Fenwick.html),
  which also discusses segment trees, but I should write more about
  them here!

Table
-----

Here's a table summarizing the above classification scheme.  I plan to
fill in links as I write blog posts about each row.

<figure class="fullwidth">

| Sequence | Ranges         | Operation            | Solution                | Precomputation   | Queries        |
|:---------|:---------------|:---------------------|:------------------------|:-----------------|:---------------|
| Static   | Sliding window | Monoid               | [Amortized queue][am-q] | $O(1)$           | $O(1)$         |
| Static   | Arbitrary      | Group                | [Prefix sum table][ps]  | $O(n)$           | $O(1)$         |
| Static   | Arbitrary      | Idempotent semigroup | [Sparse table][st]      | $O(n \lg n)$     | $O(1)$         |
| Static   | Arbitrary      | Monoid               | Sqrt tree               | $O(n \lg \lg n)$ | $O(\lg \lg n)$ |
| Dynamic  | Arbitrary      | Group                | Fenwick tree            | $O(n)$           | $O(\lg n)$     |
| Dynamic  | Arbitrary      | Monoid               | Segment tree            | $O(n)$           | $O(\lg n)$     |

</figure>

[am-q]: https://byorgey.github.io/blog/posts/2024/11/27/stacks-queues.html
[ps]: https://byorgey.github.io/blog/posts/2025/06/27/prefix-sums.html
[st]: https://byorgey.github.io/blog/posts/2025/07/18/sparse-table.html
