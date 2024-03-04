---
katex: true
title: 'Competitive programming in Haskell: sorting tree shapes'
published: 2020-05-19T18:45:17Z
categories: competitive programming,haskell
tags: ceiling,functor,Kattis,set,shape,sort,tree
---

<p>In my previous post I challenged you to solve this problem, which essentially asks how many distinct binary tree shapes are created when we take lists of numbers and build a tree from each by repeated binary search tree insertion.</p>
<div style="text-align:center;">
<p><a href="https://open.kattis.com/problems/ceiling">Ceiling Function</a></p>
</div>
<p>Incidentally, this problem was from the 2016 ICPC world finals (probably one of the easiest ICPC world finals problems ever!).</p>
<p>Several commenters solved it, and all with essentially the same solution. First we need to build some binary search trees by repeated insertion, which we can do by creating a binary tree type and insertion function and then doing some left folds over the input lists. Next, we need to classify the resulting trees by their <em>shape</em>. One obvious method would be to write a function which compares two binary trees to see if they have the same shape; use <code>nubBy</code> to remove duplicates; then count how many trees remain. This would take $O(n^2)$ time, but since there are only at most $50$ trees, with at most $20$ values in each, this should easily fit within the very geneous time limit of 5 seconds. (This is an understatement; my implementation of this approach runs in 0.01s.)</p>
<p>However, there’s a different solution which is both asymptotically faster <em>and</em> less code! The key idea is that if we make the <code>Tree</code> type polymorphic, and an instance of <code>Functor</code> (by writing our own instance, or, even better, using the <code>DeriveFunctor</code> extension), then after building the trees we can turn them into literal tree <em>shapes</em> by replacing the values they contain with <code>()</code>. Moreover, since GHC can also derive an <code>Ord</code> instance for our <code>Tree</code> type, we can then count the distinct tree shapes in $O(n \lg n)$ time, either using a combination of <code>sort</code>, <code>group</code>, and <code>length</code>, or by throwing them all into a <code>Set</code> and asking for its <code>size</code>.</p>
<p>Here’s my solution:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:green;">{-# LANGUAGE DeriveFunctor #-}</span>

<span style="color:blue;font-weight:bold;">import</span> <span>Control.Arrow</span>
<span style="color:blue;font-weight:bold;">import</span> <span>Data.List</span>

<span>main</span> <span style="color:red;">=</span> <span>interact</span> <span>$</span>
  <span>lines</span> <span>&gt;&gt;&gt;</span> <span>drop</span> <span class="hs-num">1</span> <span>&gt;&gt;&gt;</span> <span>map</span> <span style="color:red;">(</span><span>words</span> <span>&gt;&gt;&gt;</span> <span>map</span> <span>read</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span> <span>solve</span> <span>&gt;&gt;&gt;</span> <span>show</span>

<span>solve</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span style="color:red;">[</span><span>Int</span><span style="color:red;">]</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>Int</span>
<span>solve</span> <span style="color:red;">=</span> <span>map</span> <span style="color:red;">(</span><span>foldl'</span> <span style="color:red;">(</span><span>flip</span> <span>ins</span><span style="color:red;">)</span> <span>Empty</span> <span>&gt;&gt;&gt;</span> <span style="color:red;">(</span><span>()</span> <span>&lt;$</span><span style="color:red;">)</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span> <span>sort</span> <span>&gt;&gt;&gt;</span> <span>group</span> <span>&gt;&gt;&gt;</span> <span>length</span>
                                           <span style="color:green;">-- or: &gt;&gt;&gt; S.fromList &gt;&gt;&gt; S.size</span>

<span style="color:blue;font-weight:bold;">data</span> <span>Tree</span> <span>a</span> <span style="color:red;">=</span> <span>Empty</span> <span style="color:red;">|</span> <span>Node</span> <span>a</span> <span style="color:red;">(</span><span>Tree</span> <span>a</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>Tree</span> <span>a</span><span style="color:red;">)</span>
  <span style="color:blue;font-weight:bold;">deriving</span> <span style="color:red;">(</span><span>Show</span><span style="color:red;">,</span> <span>Eq</span><span style="color:red;">,</span> <span>Ord</span><span style="color:red;">,</span> <span>Functor</span><span style="color:red;">)</span>

<span>ins</span> <span style="color:red;">::</span> <span>Ord</span> <span>a</span> <span style="color:red;">=&gt;</span> <span>a</span> <span style="color:red;">-&gt;</span> <span>Tree</span> <span>a</span> <span style="color:red;">-&gt;</span> <span>Tree</span> <span>a</span>
<span>ins</span> <span>a</span> <span>Empty</span> <span style="color:red;">=</span> <span>Node</span> <span>a</span> <span>Empty</span> <span>Empty</span>
<span>ins</span> <span>a</span> <span style="color:red;">(</span><span>Node</span> <span>x</span> <span>l</span> <span>r</span><span style="color:red;">)</span>
  <span style="color:red;">|</span> <span>a</span> <span>&lt;</span> <span>x</span>     <span style="color:red;">=</span> <span>Node</span> <span>x</span> <span style="color:red;">(</span><span>ins</span> <span>a</span> <span>l</span><span style="color:red;">)</span> <span>r</span>
  <span style="color:red;">|</span> <span>otherwise</span> <span style="color:red;">=</span> <span>Node</span> <span>x</span> <span>l</span> <span style="color:red;">(</span><span>ins</span> <span>a</span> <span>r</span><span style="color:red;">)</span></code></pre>
<p>Honestly I’m not sure what the nicest way to solve this problem in something like Java or C++ would be. In Java, I suppose we would have to make a class for trees, implement <code>equals</code> and <code>compareTo</code> methods which compare trees by shape, and then put all the trees in a <code>TreeSet</code>; or else we could implement <code>hashCode</code> instead of <code>compareTo</code> and use a <code>HashSet</code>. The thing that makes the Haskell solution so much nicer is that the compiler writes some of the code for us, in the form of derived <code>Functor</code> and <code>Ord</code> instances.</p>
<p>For Friday, I invite you to solve <a href="https://open.kattis.com/problems/subway">Subway Tree System</a>, a nifty problem which is more difficult but has some similar features!</p>

