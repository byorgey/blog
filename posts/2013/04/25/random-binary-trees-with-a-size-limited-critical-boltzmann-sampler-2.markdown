---
title: Random binary trees with a size-limited critical Boltzmann sampler
published: 2013-04-25T21:20:31Z
categories: combinatorics,haskell,math,species
tags: Boltzmann,generation,QuickCheck,random,sampler,tree
---

<p>Today I’d like to talk about generating random trees. First, some imports and such (this post is literate Haskell).</p>
<pre><code><span>&gt;</span> <span style="color:green;">{-# LANGUAGE GeneralizedNewtypeDeriving #-}</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">module</span> <span>BoltzmannTrees</span> <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span> 
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Control</span><span>.</span><span>Applicative</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Control</span><span>.</span><span>Arrow</span>                  <span style="color:red;">(</span><span style="color:red;">(</span><span>&amp;&amp;&amp;</span><span style="color:red;">)</span><span style="color:red;">)</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Control</span><span>.</span><span>Lens</span>                   <span style="color:red;">(</span><span style="color:red;">(</span><span>??</span><span style="color:red;">)</span><span style="color:red;">)</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Control</span><span>.</span><span>Monad</span><span>.</span><span>Random</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Control</span><span>.</span><span>Monad</span><span>.</span><span>Reader</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Control</span><span>.</span><span>Monad</span><span>.</span><span>State</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Control</span><span>.</span><span>Monad</span><span>.</span><span>Trans</span><span>.</span><span>Maybe</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Data</span><span>.</span><span>List</span>                      <span style="color:red;">(</span><span>sort</span><span style="color:red;">)</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>Data</span><span>.</span><span>Maybe</span>                     <span style="color:red;">(</span><span>fromJust</span><span style="color:red;">)</span>
<span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span>           <span>System</span><span>.</span><span>Environment</span>             <span style="color:red;">(</span><span>getArgs</span><span style="color:red;">)</span>
</code></pre>
<p>So here’s a simple type of binary tree shapes, containing no data:</p>
<pre><code><span>&gt;</span> <span style="color:blue;font-weight:bold;">data</span> <span>Tree</span> <span style="color:red;">=</span> <span>Leaf</span> <span style="color:red;">|</span> <span>Branch</span> <span>Tree</span> <span>Tree</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">deriving</span> <span>Show</span>
</code></pre>
<p>We’ll count each constructor (<code>Leaf</code> or <code>Branch</code>) as having a size of 1:</p>
<pre><code><span>&gt;</span> <span>size</span> <span style="color:red;">::</span> <span>Tree</span> <span style="color:red;">-&gt;</span> <span>Int</span>
<span>&gt;</span> <span>size</span> <span>Leaf</span> <span style="color:red;">=</span> <span class="hs-num">1</span>
<span>&gt;</span> <span>size</span> <span style="color:red;">(</span><span>Branch</span> <span>l</span> <span>r</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span class="hs-num">1</span> <span>+</span> <span>size</span> <span>l</span> <span>+</span> <span>size</span> <span>r</span>
</code></pre>
<p>Now, suppose we want to randomly generate these trees. This is an entirely reasonable and useful thing to do: perhaps we want to, say, randomly test properties of functions over <code>Tree</code> using <a href="http://hackage.haskell.org/package/QuickCheck"><code>QuickCheck</code></a>. Here’s the simplest, most naïve way to do it:</p>
<pre><code><span>&gt;</span> <span>randomTree</span> <span style="color:red;">::</span> <span style="color:red;">(</span><span>Applicative</span> <span>m</span><span style="color:red;">,</span> <span>MonadRandom</span> <span>m</span><span style="color:red;">)</span> <span style="color:red;">=&gt;</span> <span>m</span> <span>Tree</span>
<span>&gt;</span> <span>randomTree</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>   <span>r</span> <span style="color:red;">&lt;-</span> <span>getRandom</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">if</span> <span>r</span> <span>&lt;</span> <span style="color:red;">(</span><span class="hs-num">1</span><span>/</span><span class="hs-num">2</span> <span style="color:red;">::</span> <span>Double</span><span style="color:red;">)</span>
<span>&gt;</span>     <span style="color:blue;font-weight:bold;">then</span> <span>return</span> <span>Leaf</span>
<span>&gt;</span>     <span style="color:blue;font-weight:bold;">else</span> <span>Branch</span> <span>&lt;$&gt;</span> <span>randomTree</span> <span>&lt;*&gt;</span> <span>randomTree</span>
</code></pre>
<p>We choose each of the constructors with probability $latex 1/2$, and recurse in the <code>Branch</code> case.</p>
<p>Now, <a href="http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html">as is well-known</a>, this works rather poorly. Why is that? Let’s generate 100 random trees and print out their sizes in descending order:</p>
<pre><code><span style="color:gray;">ghci&gt; </span>reverse . sort . map size &lt;$&gt; replicateM 100 randomTree
  [118331,7753,2783,763,237,203,195,163,159,73,65,63,49,41,39,29,29,23,23,21,19,19,15,11,9,9,9,9,7,7,7,5,5,5,5,5,5,5,5,5,3,3,3,3,3,3,3,3,3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
</code></pre>
<p>As you can see, this is a really weird distribution of sizes. For one thing, we get lots of trees that are very small—in fact, it’s easy to see that we expect about 50 of them to be single leaf nodes. The other weird thing, however, is that we also get some really humongous trees. The above output gets randomly regenerated every time I process this post—so I don’t know exactly what sizes you’ll end up seeing—but it’s a good bet that there is at least one tree with a size greater than $latex 10^4$. To get an intuitive idea of why this happens, imagine generating the tree in a breadth-first manner. At each new level we have a collection of “active” nodes corresponding to pending recursive calls to <code>randomTree</code>. Each active node generates zero or two new active nodes on the next level with equal probability, so <em>on average</em> the number of active nodes remains the same from level to level. So if we happen to make a lot of <code>Branch</code> choices right off the bat, it may take a long time before the tree “thins out” again. And if this distribution didn’t seem weird enough already, it turns out (though it is far from obvious how to prove this) that the <em>expected</em> size of the generated trees is <em>infinite</em>!</p>
<p>The usual solution with <code>QuickCheck</code> is to use the <code>sized</code> combinator to limit the size of generated structures, but this does not help with the problem of having too many very small trees.</p>
<p>Here’s a (seemingly!) stupid idea. Suppose we want to generate trees of size approximately 100 (say, within 10%). Let’s simply use the above algorithm, but with the following modifications:</p>
<ol style="list-style-type:decimal;">
<li>If we generate a tree of size &lt; 90, throw it away and start over.</li>
<li>If we generate a tree of size &gt; 110, throw it away and start over. As an optimization, however, we will stop <em>as soon as the size goes over 110</em>; that is, we will keep track of the current size while generating and stop early if the size gets too big.</li>
</ol>
<p>Here’s some code. First, a monad onion:</p>
<pre><code><span>&gt;</span> <span style="color:blue;font-weight:bold;">newtype</span> <span>GenM</span> <span>a</span> <span style="color:red;">=</span> <span>GenM</span> 
<span>&gt;</span>     <span style="color:red;">{</span> <span>unGenM</span> <span style="color:red;">::</span> <span>ReaderT</span> <span style="color:red;">(</span><span>Int</span><span style="color:red;">,</span><span>Int</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>StateT</span> <span>Int</span> <span style="color:red;">(</span><span>MaybeT</span> <span style="color:red;">(</span><span>Rand</span> <span>StdGen</span><span style="color:red;">)</span><span style="color:red;">)</span><span style="color:red;">)</span> <span>a</span> <span style="color:red;">}</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">deriving</span> <span style="color:red;">(</span><span>Functor</span><span style="color:red;">,</span> <span>Applicative</span><span style="color:red;">,</span> <span>Monad</span><span style="color:red;">,</span> <span>MonadPlus</span><span style="color:red;">,</span> <span>MonadRandom</span><span style="color:red;">,</span>
<span>&gt;</span>             <span>MonadState</span> <span>Int</span><span style="color:red;">,</span> <span>MonadReader</span> <span style="color:red;">(</span><span>Int</span><span style="color:red;">,</span><span>Int</span><span style="color:red;">)</span><span style="color:red;">)</span>
</code></pre>
<p>The <code>ReaderT</code> holds the min and max allowed sizes; the <code>StateT</code> holds the current size; the <code>MaybeT</code> allows for possible failure (if the tree gets too big or ends up too small), and the <code>Rand StdGen</code> is, of course, for generating random numbers. To run a computation in this monad we take a target size and a tolerance and use them to compute minimum and maximum sizes. (The <code>(??)</code> in the code below is an infix version of <code>flip</code>, defined in the <code>lens</code> package.)</p>
<pre><code><span>&gt;</span> <span>runGenM</span> <span style="color:red;">::</span> <span>Int</span> <span style="color:red;">-&gt;</span> <span>Double</span> <span style="color:red;">-&gt;</span> <span>GenM</span> <span>a</span> <span style="color:red;">-&gt;</span> <span>IO</span> <span style="color:red;">(</span><span>Maybe</span> <span>a</span><span style="color:red;">)</span>
<span>&gt;</span> <span>runGenM</span> <span>targetSize</span> <span>eps</span> <span>m</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">let</span> <span>wiggle</span>  <span style="color:red;">=</span> <span>floor</span> <span>$</span> <span>fromIntegral</span> <span>targetSize</span> <span>*</span> <span>eps</span>
<span>&gt;</span>       <span>minSize</span> <span style="color:red;">=</span> <span>targetSize</span> <span style="color:green;">-</span> <span>wiggle</span>
<span>&gt;</span>       <span>maxSize</span> <span style="color:red;">=</span> <span>targetSize</span> <span>+</span> <span>wiggle</span>
<span>&gt;</span>   <span>g</span> <span style="color:red;">&lt;-</span> <span>newStdGen</span>
<span>&gt;</span>   <span>return</span> <span>.</span> <span style="color:red;">(</span><span>evalRand</span> <span>??</span> <span>g</span><span style="color:red;">)</span> <span>.</span> <span>runMaybeT</span> <span>.</span> <span style="color:red;">(</span><span>evalStateT</span> <span>??</span> <span class="hs-num">0</span><span style="color:red;">)</span>
<span>&gt;</span>          <span>.</span> <span style="color:red;">(</span><span>runReaderT</span> <span>??</span> <span style="color:red;">(</span><span>minSize</span><span style="color:red;">,</span> <span>maxSize</span><span style="color:red;">)</span><span style="color:red;">)</span> <span>.</span> <span>unGenM</span>
<span>&gt;</span>          <span>$</span> <span>m</span>
</code></pre>
<p>Here’s the code to try generating a tree: we call the <code>atom</code> function to record the increase in size, and choose between the two constructors with equal probability. <code>atom</code>, in turn, handles failing early if the size gets too big.</p>
<pre><code><span>&gt;</span> <span>genTreeUB</span> <span style="color:red;">::</span> <span>GenM</span> <span>Tree</span>
<span>&gt;</span> <span>genTreeUB</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>   <span>r</span> <span style="color:red;">&lt;-</span> <span>getRandom</span>
<span>&gt;</span>   <span>atom</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">if</span> <span>r</span> <span>&lt;=</span> <span style="color:red;">(</span><span class="hs-num">1</span><span>/</span><span class="hs-num">2</span> <span style="color:red;">::</span> <span>Double</span><span style="color:red;">)</span>
<span>&gt;</span>     <span style="color:blue;font-weight:bold;">then</span> <span>return</span> <span>Leaf</span>
<span>&gt;</span>     <span style="color:blue;font-weight:bold;">else</span> <span>Branch</span> <span>&lt;$&gt;</span> <span>genTreeUB</span> <span>&lt;*&gt;</span> <span>genTreeUB</span>
<span>&gt;</span> 
<span>&gt;</span> <span>atom</span> <span style="color:red;">::</span> <span>GenM</span> <span>()</span>
<span>&gt;</span> <span>atom</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>   <span style="color:red;">(</span><span style="color:blue;font-weight:bold;">_</span><span style="color:red;">,</span> <span>maxSize</span><span style="color:red;">)</span> <span style="color:red;">&lt;-</span> <span>ask</span>
<span>&gt;</span>   <span>curSize</span> <span style="color:red;">&lt;-</span> <span>get</span>
<span>&gt;</span>   <span>when</span> <span style="color:red;">(</span><span>curSize</span> <span>&gt;=</span> <span>maxSize</span><span style="color:red;">)</span> <span>mzero</span>
<span>&gt;</span>   <span>put</span> <span style="color:red;">(</span><span>curSize</span> <span>+</span> <span class="hs-num">1</span><span style="color:red;">)</span>
</code></pre>
<p><code>genTreeLB</code> calls <code>genTreeUB</code> and then performs the lower bound check on the size.</p>
<pre><code><span>&gt;</span> <span>genTreeLB</span> <span style="color:red;">::</span> <span>GenM</span> <span>Tree</span>
<span>&gt;</span> <span>genTreeLB</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
<span>&gt;</span>   <span>put</span> <span class="hs-num">0</span>
<span>&gt;</span>   <span>t</span> <span style="color:red;">&lt;-</span> <span>genTreeUB</span>
<span>&gt;</span>   <span>tSize</span> <span style="color:red;">&lt;-</span> <span>get</span>
<span>&gt;</span>   <span style="color:red;">(</span><span>minSize</span><span style="color:red;">,</span> <span style="color:blue;font-weight:bold;">_</span><span style="color:red;">)</span> <span style="color:red;">&lt;-</span> <span>ask</span>
<span>&gt;</span>   <span>guard</span> <span>$</span> <span>tSize</span> <span>&gt;=</span> <span>minSize</span>
<span>&gt;</span>   <span>return</span> <span>t</span>
</code></pre>
<p>Finally, <code>genTree</code> just calls <code>genTreeLB</code> repeatedly until it succeeds.</p>
<pre><code><span>&gt;</span> <span>genTree</span> <span style="color:red;">::</span> <span>GenM</span> <span>Tree</span>
<span>&gt;</span> <span>genTree</span> <span style="color:red;">=</span> <span>genTreeLB</span> <span>`mplus`</span> <span>genTree</span>
</code></pre>
<p>Let’s make sure it works:</p>
<pre><code><span style="color:gray;">ghci&gt; </span>map size . fromJust &lt;$&gt; runGenM 100 0.1 (replicateM 30 genTree)
  [105,91,105,103,107,101,105,93,93,93,95,91,103,91,91,107,105,103,97,95,105,107,93,97,93,103,91,103,101,95]
</code></pre>
<p>Neat! Okay, but surely this is really, really slow, right? We spend a bunch of time just throwing away trees of the wrong size. Before reading on, would you care to guess the asymptotic time complexity to generate a tree of size $latex n$ using this algorithm?</p>
<p>And while you think about that, here is a random binary tree of size approximately 1000.</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2013/04/5f8af30e809e8b0fa48b9a1a8eaf3a64.png" /></p>
</div>
<p>And the answer is… it is <em>linear</em>! That is, it takes $latex O(n)$ time to generate a tree of size $latex n$. This is astounding—it’s the best we could possibly hope for, because of course it takes at least $latex O(n)$ time to generate an object of size $latex O(n)$. If you don’t believe me, I invite you to run some experiments with this code yourself. I did, and it sure looks linear:</p>
<pre><code>main = do
  [sz] &lt;- getArgs
  Just ts &lt;- runGenM (read sz) 0.1 $ replicateM 1000 genTree
  print . (/fromIntegral n) . fromIntegral . sum . map size $ ts

archimedes :: research/species/boltzmann » time ./GenTree 50
49.682
./GenTree 50  1.37s user 0.01s system 99% cpu 1.387 total
archimedes :: research/species/boltzmann » time ./GenTree 100
99.474
./GenTree 100  3.11s user 0.02s system 99% cpu 3.152 total
archimedes :: research/species/boltzmann » time ./GenTree 200
198.494
./GenTree 200  6.82s user 0.04s system 99% cpu 6.876 total
archimedes :: research/species/boltzmann » time ./GenTree 400
398.798
./GenTree 400  13.08s user 0.08s system 99% cpu 13.208 total
archimedes :: research/species/boltzmann » time ./GenTree 800
795.798
./GenTree 800  25.99s user 0.16s system 99% cpu 26.228 total</code></pre>
<p>The proof of this astounding fact uses some <em>complex analysis</em> which I do not understand; I wish I was joking. Of course, the constant factor can be big, depending on how small you set the “epsilon” allowing for wiggle room around the target size.<sup><a href="#fn1" class="footnoteRef" id="fnref1">1</a></sup> But it is still quite feasible to generate rather large trees (with, say, $latex 10^5$ nodes).</p>
<p>There is much, much more to say on this topic. I just wanted to start out with a simple example before jumping into more of the technical details and generalizations, which I plan to write about in future posts. I also hope to package this and a bunch of other stuff into a library. In the meantime, you can read Duchon <em>et. al</em><sup><a href="#fn2" class="footnoteRef" id="fnref2">2</a></sup> if you want the details.</p>
<div class="footnotes">
<hr />
<ol>
<li id="fn1"><p>Actually, if you set epsilon to zero, the asymptotic complexity jumps to $latex O(n^2)$.<a href="#fnref1">↩</a></p></li>
<li id="fn2"><p>Duchon, Philippe, <em>et al.</em> “Boltzmann samplers for the random generation of combinatorial structures.” Combinatorics Probability and Computing 13.4-5 (2004): 577-625.<a href="#fnref2">↩</a></p></li>
</ol>
</div>

