---
title: 'Deriving pleasure from GHC 6.12.1'
published: 2010-03-03T23:16:37Z
categories: haskell
tags: 
---

<p><a href="http://haskell.org/ghc/download_ghc_6_12_1.html">GHC 6.12.1</a> has been out for a few months now, and things are starting to come together in terms of library compatibility. I look forward to the next release of the <a href="http://hackage.haskell.org/platform/">Haskell Platform</a> which will include 6.12.</p><p>Here's a totally sweet new feature in GHC 6.12.1 which you might not be aware of (I wasn't aware of it for quite a while after the release). Let's suppose I've defined the following data type of n-way trees with data at the leaves:</p><pre><code><span>&gt;</span> <span style="color:blue;font-weight:bold;">data</span> <span>Tree</span> <span>a</span> <span style="color:red;">=</span> <span>Leaf</span> <span>a</span> <span style="color:red;">|</span> <span>Node</span> <span style="color:red;">[</span><span>Tree</span> <span>a</span><span style="color:red;">]</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">deriving</span> <span>Show</span>
</code></pre><p>For example, here's an example value of this type:</p><pre><code><span>&gt;</span> <span>t</span> <span style="color:red;">::</span> <span>Tree</span> <span>Integer</span>
<span>&gt;</span> <span>t</span> <span style="color:red;">=</span> <span>Node</span> <span style="color:red;">[</span><span>Leaf</span> <span class="hs-num">3</span><span style="color:red;">,</span> <span>Leaf</span> <span class="hs-num">7</span><span style="color:red;">,</span> <span>Node</span> <span style="color:red;">[</span><span>Leaf</span> <span class="hs-num">4</span><span style="color:red;">,</span> <span>Leaf</span> <span class="hs-num">8</span><span style="color:red;">]</span><span style="color:red;">,</span> <span>Leaf</span> <span class="hs-num">1</span><span style="color:red;">]</span>
</code></pre><p>Now, suppose we wanted to do something to every leaf in a tree, (say) increment all the values by one. Of course, the best way to do this is to make <code>Tree</code> an instance of <code>Functor</code>, like so:</p><pre><code><span>&gt;</span> <span style="color:blue;font-weight:bold;">instance</span> <span>Functor</span> <span>Tree</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>   <span>fmap</span> <span>f</span> <span style="color:red;">(</span><span>Leaf</span> <span>x</span><span style="color:red;">)</span>  <span style="color:red;">=</span> <span>Leaf</span> <span style="color:red;">(</span><span>f</span> <span>x</span><span style="color:red;">)</span>
<span>&gt;</span>   <span>fmap</span> <span>f</span> <span style="color:red;">(</span><span>Node</span> <span>ts</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>Node</span> <span style="color:red;">(</span><span>fmap</span> <span style="color:red;">(</span><span>fmap</span> <span>f</span><span style="color:red;">)</span> <span>ts</span><span style="color:red;">)</span>
</code></pre><p>Now we can increment every leaf in <code>t</code>:</p><pre><code>ghci&gt; fmap (+1) t
Node [Leaf 4,Leaf 8,Node [Leaf 5,Leaf 9],Leaf 2]</code></pre><p>OK, that wasn't so hard. But... that <code>Functor</code> instance was awfully boring to write (even if it wasn't really <em>that</em> long). In a sense it is &quot;obvious&quot; what the <code>Functor</code> instance should do just from looking at the definition of <code>Tree</code>. Couldn't the <code>Functor</code> instance be automatically generated somehow?</p><p>It certainly can, and in the past the answer would have been &quot;use a tool like <a href="http://repetae.net/computer/haskell/DrIFT/">DrIFT</a> or <a href="http://community.haskell.org/~ndm/derive/">Derive</a>&quot;. However... GHC can now do it for you! All we need is to enable the <code>DeriveFunctor</code> extension, and add <code>Functor</code> to the <code>deriving</code> clause after the definition of our data type. Like this:</p><pre><code><span>&gt;</span> <span style="color:green;">{-# LANGUAGE DeriveFunctor #-}</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">data</span> <span>Tree</span> <span>a</span> <span style="color:red;">=</span> <span>Leaf</span> <span>a</span> <span style="color:red;">|</span> <span>Node</span> <span style="color:red;">[</span><span>Tree</span> <span>a</span><span style="color:red;">]</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">deriving</span> <span style="color:red;">(</span><span>Show</span><span style="color:red;">,</span> <span>Functor</span><span style="color:red;">)</span>
<span>&gt;</span> 
<span>&gt;</span> <span>t</span> <span style="color:red;">::</span> <span>Tree</span> <span>Integer</span>
<span>&gt;</span> <span>t</span> <span style="color:red;">=</span> <span>Node</span> <span style="color:red;">[</span><span>Leaf</span> <span class="hs-num">3</span><span style="color:red;">,</span> <span>Leaf</span> <span class="hs-num">7</span><span style="color:red;">,</span> <span>Node</span> <span style="color:red;">[</span><span>Leaf</span> <span class="hs-num">4</span><span style="color:red;">,</span> <span>Leaf</span> <span class="hs-num">8</span><span style="color:red;">]</span><span style="color:red;">,</span> <span>Leaf</span> <span class="hs-num">1</span><span style="color:red;">]</span>
</code></pre><p>And voila! No silly boilerplate. It Just Works:</p><pre><code>ghci&gt; fmap (+1) t
Node [Leaf 4,Leaf 8,Node [Leaf 5,Leaf 9],Leaf 2]</code></pre><p>And that's not all! GHC can automatically derive <a href="http://haskell.org/ghc/docs/latest/html/libraries/base-4.2.0.0/Data-Foldable.html">Foldable</a> instances for us as well. We just need to add one more extension, and import <code>Data.Foldable</code>. Let's import <code>Data.Monoid</code> too, just for fun.</p><pre><code><span>&gt;</span> <span style="color:green;">{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span>Data</span><span>.</span><span>Foldable</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span>Data</span><span>.</span><span>Monoid</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">data</span> <span>Tree</span> <span>a</span> <span style="color:red;">=</span> <span>Leaf</span> <span>a</span> <span style="color:red;">|</span> <span>Node</span> <span style="color:red;">[</span><span>Tree</span> <span>a</span><span style="color:red;">]</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">deriving</span> <span style="color:red;">(</span><span>Show</span><span style="color:red;">,</span> <span>Functor</span><span style="color:red;">,</span> <span>Foldable</span><span style="color:red;">)</span>
<span>&gt;</span> 
<span>&gt;</span> <span>t</span> <span style="color:red;">::</span> <span>Tree</span> <span>Integer</span>
<span>&gt;</span> <span>t</span> <span style="color:red;">=</span> <span>Node</span> <span style="color:red;">[</span><span>Leaf</span> <span class="hs-num">3</span><span style="color:red;">,</span> <span>Leaf</span> <span class="hs-num">7</span><span style="color:red;">,</span> <span>Node</span> <span style="color:red;">[</span><span>Leaf</span> <span class="hs-num">4</span><span style="color:red;">,</span> <span>Leaf</span> <span class="hs-num">8</span><span style="color:red;">]</span><span style="color:red;">,</span> <span>Leaf</span> <span class="hs-num">1</span><span style="color:red;">]</span>
</code></pre><p>Behold:</p><pre><code>ghci&gt; foldMap Sum t
Sum {getSum = 23}
ghci&gt; foldMap (:[]) t
[3,7,4,8,1]</code></pre><p>Sweet! We can add up all the leaves in a tree, or flatten a tree into a list, and a ton of other useful operations besides, with pretty much zero work on our part.</p><p>But wait! There's <em>even more</em>! GHC can derive <a href="http://haskell.org/ghc/docs/latest/html/libraries/base-4.2.0.0/Data-Traversable.html">Traversable</a> too.</p><pre><code><span>&gt;</span> <span style="color:green;">{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span>Data</span><span>.</span><span>Foldable</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span>Data</span><span>.</span><span>Monoid</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span>Data</span><span>.</span><span>Traversable</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">data</span> <span>Tree</span> <span>a</span> <span style="color:red;">=</span> <span>Leaf</span> <span>a</span> <span style="color:red;">|</span> <span>Node</span> <span style="color:red;">[</span><span>Tree</span> <span>a</span><span style="color:red;">]</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">deriving</span> <span style="color:red;">(</span><span>Show</span><span style="color:red;">,</span> <span>Functor</span><span style="color:red;">,</span> <span>Foldable</span><span style="color:red;">,</span> <span>Traversable</span><span style="color:red;">)</span>
<span>&gt;</span> 
<span>&gt;</span> <span>t</span> <span style="color:red;">::</span> <span>Tree</span> <span>Integer</span>
<span>&gt;</span> <span>t</span> <span style="color:red;">=</span> <span>Node</span> <span style="color:red;">[</span><span>Leaf</span> <span class="hs-num">3</span><span style="color:red;">,</span> <span>Leaf</span> <span class="hs-num">7</span><span style="color:red;">,</span> <span>Node</span> <span style="color:red;">[</span><span>Leaf</span> <span class="hs-num">4</span><span style="color:red;">,</span> <span>Leaf</span> <span class="hs-num">8</span><span style="color:red;">]</span><span style="color:red;">,</span> <span>Leaf</span> <span class="hs-num">1</span><span style="color:red;">]</span>
</code></pre><pre><code>ghci&gt; traverse (\x -&gt; if x &gt; 6 then [x-1, x] else [x]) t
[ Node [Leaf 3,Leaf 6,Node [Leaf 4,Leaf 7],Leaf 1]
, Node [Leaf 3,Leaf 6,Node [Leaf 4,Leaf 8],Leaf 1]
, Node [Leaf 3,Leaf 7,Node [Leaf 4,Leaf 7],Leaf 1]
, Node [Leaf 3,Leaf 7,Node [Leaf 4,Leaf 8],Leaf 1]
]</code></pre><p>A wesome! This example is a bit silly, perhaps, but I'll leave you to come up with more interesting uses of <code>traverse</code>.</p><p>I should point out that this certainly doesn't <em>replace</em> tools like <a href="http://repetae.net/computer/haskell/DrIFT/">DrIFT</a> and <a href="http://community.haskell.org/~ndm/derive/">Derive</a>, which can do tons of other things as well. But having baked-in support for these very common cases is sure convenient!</p>

