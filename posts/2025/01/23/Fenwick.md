---
title: 'You could have invented Fenwick trees'
tags: Haskell,segment,Fenwick,tree,JFP,journal,paper
bibliography: Fenwick.bib
katex: true
---

My paper, [You could have invented Fenwick
trees](http://ozark.hendrix.edu/~yorgey/pub/Fenwick-ext.pdf), has just
been published as a [Functional Pearl in the Journal of Functional
Programming](https://www.cambridge.org/core/services/aop-cambridge-core/content/view/B4628279D4E54229CED97249E96F721D/S0956796824000169a.pdf/you-could-have-invented-fenwick-trees.pdf).
This blog post is an advertisement for the paper, which presents a
novel way to derive the *Fenwick tree* data structure from first
principles.

Suppose we have a sequence of integers $a_1, \dots, a_n$ and want to be
able to perform two operations:

* we can *update* any $a_i$ by adding some value $v$ to it; or
* we can perform a *range query*, which asks for the sum of the values
  $a_i + \dots + a_j$ for any range $[i,j]$.

There are several ways to solve this problem. For example:

1. We could just keep the sequence of integers in a mutable array.
   Updating is $O(1)$, but range queries are $O(n)$ since we must
   actually loop through the range and add up all the values.
2. We could keep a separate array of *prefix sums* on the side, so
   that $P_i$ stores the sum $a_1 + \dots + a_i$.  Then the range
   query on $[i,j]$ can be computed as $P_j - P_{i-1}$, which only
   takes $O(1)$; however, updates now take $O(n)$ since we must also
   update all the prefix sums which include the updated element.
3. We can get the best of both worlds using a *segment tree*, a binary
   tree storing the elements at the leaves, with each internal node
   caching the sum of its children.  Then both update and range query
   can be done in $O(\lg n)$.

I won't go through the details of this third solution here, but it is
relatively straightforward to understand and implement, especially in
a functional language.

However, there is a fourth solution, known as a *Fenwick tree* or
*Fenwick array*, independently invented by @ryabko1989fast and
@fenwick1994new.  Here's a typical Java implementation of a Fenwick
tree:

```.java
class FenwickTree {
    private long[] a;
    public FenwickTree(int n) { a = new long[n+1]; }
    public long prefix(int i) {
        long s = 0;
        for (; i > 0; i -= LSB(i)) s += a[i]; return s;
    }
    public void update(int i, long delta) {
        for (; i < a.length; i += LSB(i)) a[i] += delta;
    }
    public long range(int i, int j) {
        return prefix(j) - prefix(i-1);
    }
    public long get(int i) { return range(i,i); }
    public void set(int i, long v) { update(i, v - get(i)); }
    private int LSB(int i) { return i & (-i); }
}
```

I know what you're thinking: what the heck!?  There are some loops adding and
subtracting `LSB(i)`, which is defined as the bitwise AND of `i` and
`-i`?  What on earth is this doing?  Unless you have seen this
before, this code is probably a complete mystery, as it was for me the
first time I encountered it.

However, from the right point of view, we can derive this mysterious imperative
code as an optimization of segment trees.  In particular, in my
paper I show how we can:

1. Start with a segment tree.
2. Delete some redundant info from the segment tree, and shove the
   remaining values into an array in a systematic way.
3. Define operations for moving around in the resulting Fenwick array by
   converting array indices to indices in a segment tree, moving
   around the tree appropriately, and converting back.
4. Describe these operations using a Haskell EDSL for
   infinite-precision 2's complement binary arithmetic, and fuse away
   all the intermediate conversion steps, until the above mysterious
   implementation pops out.
5. Profit.

I may be exaggerating step 5 a teensy bit.  But you'll find everything
else described in much greater detail, with pretty pictures, in the
paper! The [official JFP version is here](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/you-could-have-invented-fenwick-trees/B4628279D4E54229CED97249E96F721D), and here's an [extended
version with an appendix containing an omitted proof](http://ozark.hendrix.edu/~yorgey/pub/Fenwick-ext.pdf).
