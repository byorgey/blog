---
title: Competitive programming in Haskell: cycle decomposition with mutable arrays
published: 2020-07-18T19:56:55Z
categories: competitive programming,haskell
tags: cross,geometry,Kattis
---

<p>In my <a href="https://byorgey.wordpress.com/2020/07/10/competitive-programming-in-haskell-2d-cross-product-part-1/">previous post</a> I I challenged you to solve <a href="https://open.kattis.com/problems/chairhopping">Chair Hopping</a>: if a bunch of people permute themselves according to the same rule twice, how many different rules could they be following which would result in the observed final permutation? Or, more formally, given a permutation $latex \rho$ on $latex [1 \dots n]$, how many permutations $latex \sigma$ are there such that $latex \sigma^2 = \rho$?</p>
<p>Since this has to do with permutations, it should be unsurprising that <em>cycle decomposition</em> comes into the picture. And we have <a href="https://byorgey.wordpress.com/2020/05/30/competitive-programming-in-haskell-permutations/">discussed cycle decomposition of permutations</a> before; using those techniques to decompose the given permutation into cycles should be straightforward, right?</p>
<h2 id="not-so-fast">Not so fast!</h2>
<p>Here is the code we used previously to compute the size of the cycle containing a given element:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>dist</span> <span style="color:red;">::</span> <span>Perm</span> <span style="color:red;">-&gt;</span> <span>Int</span> <span style="color:red;">-&gt;</span> <span>Int</span> <span style="color:red;">-&gt;</span> <span>Int</span>
<span>dist</span> <span>p</span> <span>i</span> <span>j</span> <span style="color:red;">=</span> <span>length</span> <span>$</span> <span>takeWhile</span> <span style="color:red;">(</span><span>/=</span> <span>j</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>iterate</span> <span style="color:red;">(</span><span>p</span><span>!</span><span style="color:red;">)</span> <span>i</span><span style="color:red;">)</span>

<span>cycleLen</span> <span style="color:red;">::</span> <span>Perm</span> <span style="color:red;">-&gt;</span> <span>Int</span> <span style="color:red;">-&gt;</span> <span>Int</span>
<span>cycleLen</span> <span>p</span> <span>i</span> <span style="color:red;">=</span> <span>succ</span> <span>$</span> <span>dist</span> <span>p</span> <span style="color:red;">(</span><span>p</span><span>!</span><span>i</span><span style="color:red;">)</span> <span>i</span></code></pre>
<p>There’s nothing particularly wrong with this code, and no way to speed it up per se. Computing the distance between $latex i$ and $latex j$ in permutation $latex p$ takes $latex O(n)$, since we may have to scan through a significant fraction of the entire permutation if $latex i$ and $latex j$ are in a large cycle. But this is unavoidable. <code>cycleLen</code> then just uses <code>dist</code>, and if all we want to do is find the length of a single cycle this is unavoidable too.</p>
<p>However, the problem comes when we want to, for example, find the length of the cycle of <em>many</em> elements. <code>cycleLen</code> will take $latex O(n)$ for <em>each</em> element we call it on. In the worst case, if the entire permutation consists of one giant cycle, calling <code>cycleLen</code> on every element will take $latex O(n^2)$ overall. And this is particularly silly since the work of following the cycle will be entirely repeated every time, only starting from a different place! When $latex n = 200$, as in <a href="https://open.kattis.com/problems/substitution">The Power of Substitution</a>, an $latex O(n^2)$ algorithm is no big deal; but when $latex n = 10^5$ it’s entirely too slow. Using $latex 10^8$ operations per second as our rule of thumb, we expect an $latex O(n^2)$ algorithm on an input with $latex n = 10^5$ to take on the order of $latex (10^5)^2 / 10^8 = 100$ seconds. An input size of $latex 10^5$ is extremely common in competitive programming problems: not so big that I/O is going to be a huge bottleneck, but big enough that you need to come up with an algorithm faster than $latex O(n^2)$ (for example, $latex O(n)$ or $latex O(n \lg n)$ are both fine).</p>
<h2 id="permutations-and-fast-cycle-decomposition">Permutations and fast cycle decomposition</h2>
<p>The idea is to do the work of decomposing a permutation into cycles only <em>once</em>, in $latex O(n)$ time, and store the results in a data structure that allows us to look up the needed information quickly. (This general technique of preprocessing some data into a structure allowing for fast subsequent query/lookup is ubiquitous in competitive programming, and indeed in all of computer science.) The catch? I don’t know of a good way to do this without using mutable arrays! But if we write it generically we can potentially reuse it (I have in fact reused this code several times already on other problems).</p>
<p>Let’s make a library for representing permutations. This code can be found in <a href="https://github.com/byorgey/comprog-hs/blob/master/Perm.hs">Perm.hs</a>. First, some imports and the main <code>Perm</code> type itself, which is just an alias for <code>UArray Int Int</code>. <code>UArray</code> represents (immutable) <a href="https://hackage.haskell.org/package/array-0.5.4.0/docs/Data-Array-Unboxed.html">unboxed arrays</a>, that is, arrays whose elements can be stored “unboxed” in a contiguous block of memory. “Boxed” arrays are those where the array actually stores pointers and the elements themselves are allocated somewhere else. Of course we prefer using unboxed arrays whenever possible!</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:green;">{-# LANGUAGE BangPatterns #-}</span>

<span style="color:blue;font-weight:bold;">module</span> <span>Perm</span> <span style="color:blue;font-weight:bold;">where</span>

<span style="color:blue;font-weight:bold;">import</span>           <span>Control.Arrow</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Control.Monad.ST</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Data.Array.Base</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Data.Array.MArray</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Data.Array.ST</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Data.Array.Unboxed</span>

<span style="color:green;">-- | 'Perm' represents a /1-indexed/ permutation.  It can also be</span>
<span style="color:green;">--   thought of as an endofunction on the set @{1 .. n}@.</span>
<span style="color:blue;font-weight:bold;">type</span> <span>Perm</span> <span style="color:red;">=</span> <span>UArray</span> <span>Int</span> <span>Int</span></code></pre>
<p>Just based on the problems where I used it, I’ve chosen to make <code>Perm</code> values <em>1-indexed</em>, though of course we could easily have made a different choice. We can now define a few utility functions for working with permutations: <code>fromList</code> constructs a <code>Perm</code> from a list; <code>andThen</code> composes permutations; and <code>inverse</code> computes the <em>inverse</em> of a permutation. We’ll only need <code>fromList</code> to solve Chair Hopping, but the others may come in handy for other problems.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:green;">-- | Construct a 'Perm' from a list containing a permutation of the</span>
<span style="color:green;">--   numbers 1..n.  The resulting 'Perm' sends @i@ to whatever number</span>
<span style="color:green;">--   is at index @i-1@ in the list.</span>
<span>fromList</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span>Int</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>Perm</span>
<span>fromList</span> <span>xs</span> <span style="color:red;">=</span> <span>listArray</span> <span style="color:red;">(</span><span class="hs-num">1</span><span style="color:red;">,</span><span>length</span> <span>xs</span><span style="color:red;">)</span> <span>xs</span>

<span style="color:green;">-- | Compose two permutations (corresponds to backwards function</span>
<span style="color:green;">--   composition).  Only defined if the permutations have the same</span>
<span style="color:green;">--   size.</span>
<span>andThen</span> <span style="color:red;">::</span> <span>Perm</span> <span style="color:red;">-&gt;</span> <span>Perm</span> <span style="color:red;">-&gt;</span> <span>Perm</span>
<span>andThen</span> <span>p1</span> <span>p2</span> <span style="color:red;">=</span> <span>listArray</span> <span style="color:red;">(</span><span>bounds</span> <span>p1</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>map</span> <span style="color:red;">(</span><span style="color:red;">(</span><span>p1</span><span>!</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span> <span style="color:red;">(</span><span>p2</span><span>!</span><span style="color:red;">)</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>range</span> <span style="color:red;">(</span><span>bounds</span> <span>p1</span><span style="color:red;">)</span><span style="color:red;">)</span><span style="color:red;">)</span>

<span style="color:green;">-- | Compute the inverse of a permutation.</span>
<span>inverse</span> <span style="color:red;">::</span> <span>Perm</span> <span style="color:red;">-&gt;</span> <span>Perm</span>
<span>inverse</span> <span>p</span> <span style="color:red;">=</span> <span>array</span> <span style="color:red;">(</span><span>bounds</span> <span>p</span><span style="color:red;">)</span> <span style="color:red;">[</span> <span style="color:red;">(</span><span>p</span><span>!</span><span>k</span><span style="color:red;">,</span> <span>k</span><span style="color:red;">)</span> <span style="color:red;">|</span> <span>k</span> <span style="color:red;">&lt;-</span> <span>range</span> <span style="color:red;">(</span><span>bounds</span> <span>p</span><span style="color:red;">)</span> <span style="color:red;">]</span>
</code></pre>
<p>When decomposing a permutation into cycles, we assign each cycle a unique ID number, and compute a number of mappings:</p>
<ul>
<li>from each element to the ID number of its cycle;</li>
<li>from each cycle to its length;</li>
<li>from each element to its index in its cycle;</li>
<li>from each possible cycle size to the number of cycles of that size.</li>
</ul>
<p>These mappings are collected in the <code>CycleDecomp</code> data type:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:blue;font-weight:bold;">data</span> <span>CycleDecomp</span> <span style="color:red;">=</span> <span>CD</span>
  <span style="color:red;">{</span> <span>cycleID</span>     <span style="color:red;">::</span> <span>UArray</span> <span>Int</span> <span>Int</span>  <span style="color:green;">-- | Each number maps to the ID# of the cycle it is part of</span>
  <span style="color:red;">,</span> <span>cycleLen</span>    <span style="color:red;">::</span> <span>UArray</span> <span>Int</span> <span>Int</span>  <span style="color:green;">-- | Each cycle ID maps to the length of that cycle</span>
  <span style="color:red;">,</span> <span>cycleIndex</span>  <span style="color:red;">::</span> <span>UArray</span> <span>Int</span> <span>Int</span>  <span style="color:green;">-- | Each element maps to its (0-based) index in its cycle</span>
  <span style="color:red;">,</span> <span>cycleCounts</span> <span style="color:red;">::</span> <span>UArray</span> <span>Int</span> <span>Int</span>  <span style="color:green;">-- | Each size maps to the number of cycles of that size</span>
  <span style="color:red;">}</span>
  <span style="color:blue;font-weight:bold;">deriving</span> <span>Show</span></code></pre>
<p>We can use these to quickly look up information about the cycle decomposition of a permutation. For example, if we want to know the size of the cycle containing element <code>e</code>, we can look it up with <code>cycleLen!(cycleID!e)</code>. Or if we know that <code>a</code> and <code>b</code> are in the same cycle and we want to know the distance from <code>a</code> to <code>b</code>, we can compute it as <code>(cycleIndex!b - cycleIndex!a) `mod` (cycleLen!(cycleID!a))</code>.</p>
<p>Finally, here’s my code to actually compute all this information about a cycle decomposition in $latex O(n)$ time, which works by looking at each element, and when finding an element which is so far unprocessed, it does a DFS in the permutation following the cycle from that element. To be honest, it’s kind of ugly; that’s what we get for working with mutable arrays in Haskell. I am very much interested if anyone has any ideas on how to make this (1) faster or (2) prettier. (I am aware those two criteria may be at odds!) I’m using <code>STUArray</code> which allows mutation inside a monadic <code>ST</code> block; at the end we <code>freeze</code> them into normal immutable <code>UArray</code>s. (Note there are also unsafe variants of reading, writing, and freezing which do less checks, but using them didn’t seem to speed things up; I’m very open to suggestions.)</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:green;">-- | Cycle decomposition of a permutation in O(n), using mutable arrays.</span>
<span>permToCycles</span> <span style="color:red;">::</span> <span>Perm</span> <span style="color:red;">-&gt;</span> <span>CycleDecomp</span>
<span>permToCycles</span> <span>p</span> <span style="color:red;">=</span> <span>cd</span> <span style="color:blue;font-weight:bold;">where</span>

  <span style="color:red;">(</span><span style="color:blue;font-weight:bold;">_</span><span style="color:red;">,</span><span>n</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>bounds</span> <span>p</span>

  <span>cd</span> <span style="color:red;">=</span> <span>runST</span> <span>$</span> <span style="color:blue;font-weight:bold;">do</span>
    <span>cid</span> <span style="color:red;">&lt;-</span> <span>newArray</span> <span style="color:red;">(</span><span class="hs-num">1</span><span style="color:red;">,</span><span>n</span><span style="color:red;">)</span> <span class="hs-num">0</span>
    <span>cix</span> <span style="color:red;">&lt;-</span> <span>newArray</span> <span style="color:red;">(</span><span class="hs-num">1</span><span style="color:red;">,</span><span>n</span><span style="color:red;">)</span> <span class="hs-num">0</span>
    <span>ccs</span> <span style="color:red;">&lt;-</span> <span>newArray</span> <span style="color:red;">(</span><span class="hs-num">1</span><span style="color:red;">,</span><span>n</span><span style="color:red;">)</span> <span class="hs-num">0</span>

    <span>lens</span> <span style="color:red;">&lt;-</span> <span>findCycles</span> <span>cid</span> <span>cix</span> <span>ccs</span> <span class="hs-num">1</span> <span class="hs-num">1</span>
    <span>cid'</span> <span style="color:red;">&lt;-</span> <span>freeze</span> <span>cid</span>
    <span>cix'</span> <span style="color:red;">&lt;-</span> <span>freeze</span> <span>cix</span>
    <span>ccs'</span> <span style="color:red;">&lt;-</span> <span>freeze</span> <span>ccs</span>
    <span>return</span> <span>$</span> <span>CD</span> <span>cid'</span> <span style="color:red;">(</span><span>listArray</span> <span style="color:red;">(</span><span class="hs-num">1</span><span style="color:red;">,</span><span>length</span> <span>lens</span><span style="color:red;">)</span> <span>lens</span><span style="color:red;">)</span> <span>cix'</span> <span>ccs'</span>

  <span>findCycles</span> <span style="color:red;">::</span> <span>STUArray</span> <span>s</span> <span>Int</span> <span>Int</span> <span style="color:red;">-&gt;</span> <span>STUArray</span> <span>s</span> <span>Int</span> <span>Int</span> <span style="color:red;">-&gt;</span> <span>STUArray</span> <span>s</span> <span>Int</span> <span>Int</span>
    <span style="color:red;">-&gt;</span> <span>Int</span> <span style="color:red;">-&gt;</span> <span>Int</span> <span style="color:red;">-&gt;</span> <span>ST</span> <span>s</span> <span style="color:red;">[</span><span>Int</span><span style="color:red;">]</span>
  <span>findCycles</span> <span>cid</span> <span>cix</span> <span>ccs</span> <span>l</span> <span>!</span><span>k</span>   <span style="color:green;">-- l = next available cycle ID; k = cur element</span>
    <span style="color:red;">|</span> <span>k</span> <span>&gt;</span> <span>n</span>     <span style="color:red;">=</span> <span>return</span> <span>[]</span>
    <span style="color:red;">|</span> <span>otherwise</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
        <span style="color:green;">-- check if k is already marked as part of a cycle</span>
        <span>id</span> <span style="color:red;">&lt;-</span> <span>readArray</span> <span>cid</span> <span>k</span>
        <span style="color:blue;font-weight:bold;">case</span> <span>id</span> <span style="color:blue;font-weight:bold;">of</span>
          <span class="hs-num">0</span> <span style="color:red;">-&gt;</span> <span style="color:blue;font-weight:bold;">do</span>
            <span style="color:green;">-- k is unvisited.  Explore its cycle and label it as l.</span>
            <span>len</span> <span style="color:red;">&lt;-</span> <span>labelCycle</span> <span>cid</span> <span>cix</span> <span>l</span> <span>k</span> <span class="hs-num">0</span>

            <span style="color:green;">-- Remember that we have one more cycle of this size.</span>
            <span>count</span> <span style="color:red;">&lt;-</span> <span>readArray</span> <span>ccs</span> <span>len</span>
            <span>writeArray</span> <span>ccs</span> <span>len</span> <span style="color:red;">(</span><span>count</span><span>+</span><span class="hs-num">1</span><span style="color:red;">)</span>

            <span style="color:green;">-- Continue with the next label and the next element, and</span>
            <span style="color:green;">-- remember the size of this cycle</span>
            <span style="color:red;">(</span><span>len</span><span>:</span><span style="color:red;">)</span> <span>&lt;$&gt;</span> <span>findCycles</span> <span>cid</span> <span>cix</span> <span>ccs</span> <span style="color:red;">(</span><span>l</span><span>+</span><span class="hs-num">1</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>k</span><span>+</span><span class="hs-num">1</span><span style="color:red;">)</span>

          <span style="color:green;">-- k is already visited: just go on to the next element</span>
          <span style="color:blue;font-weight:bold;">_</span> <span style="color:red;">-&gt;</span> <span>findCycles</span> <span>cid</span> <span>cix</span> <span>ccs</span> <span>l</span> <span style="color:red;">(</span><span>k</span><span>+</span><span class="hs-num">1</span><span style="color:red;">)</span>

  <span style="color:green;">-- Explore a single cycle, label all its elements and return its size.</span>
  <span>labelCycle</span> <span>cid</span> <span>cix</span> <span>l</span> <span>k</span> <span>!</span><span>i</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>

    <span style="color:green;">-- Keep going as long as the next element is unlabelled.</span>
    <span>id</span> <span style="color:red;">&lt;-</span> <span>readArray</span> <span>cid</span> <span>k</span>
    <span style="color:blue;font-weight:bold;">case</span> <span>id</span> <span style="color:blue;font-weight:bold;">of</span>
      <span class="hs-num">0</span> <span style="color:red;">-&gt;</span> <span style="color:blue;font-weight:bold;">do</span>

        <span style="color:green;">-- Label the current element with l.</span>
        <span>writeArray</span> <span>cid</span> <span>k</span> <span>l</span>
        <span style="color:green;">-- The index of the current element is i.</span>
        <span>writeArray</span> <span>cix</span> <span>k</span> <span>i</span>

        <span style="color:green;">-- Look up the next element in the permutation and continue.</span>
        <span style="color:red;">(</span><span class="hs-num">1</span><span>+</span><span style="color:red;">)</span> <span>&lt;$&gt;</span> <span>labelCycle</span> <span>cid</span> <span>cix</span> <span>l</span> <span style="color:red;">(</span><span>p</span><span>!</span><span>k</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>i</span><span>+</span><span class="hs-num">1</span><span style="color:red;">)</span>
      <span style="color:blue;font-weight:bold;">_</span> <span style="color:red;">-&gt;</span> <span>return</span> <span class="hs-num">0</span></code></pre>
<p>This code is overly generic in some sense—we don’t actually need all this information to solve Chair Hopping, for example—but again, I am trying to make it as reusable as possible.</p>
<p>Now, how can we use cycle decomposition to solve Chair Hopping? That will have to wait for another post!</p>

