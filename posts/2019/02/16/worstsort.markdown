---
title: 'Worstsort'
published: 2019-02-16T20:36:50Z
categories: haskell,humor,math
tags: badsort,computable,Fun,iterated,sort,worstsort
---

<p>Thanks for the responses to my previous post about finding roots of polynomials; I now have some new avenues to explore. But today I want to write about something completely different. I recently stumbled across <a href="https://sites.math.northwestern.edu/~mlerma/papers/inefficient_algorithms.pdf">this fun paper by Miguel Lerna</a>. I realized a Haskell implementation would be very elegant, and I couldn’t pass up the opportunity to share.</p>
<h2 id="badsort">Badsort</h2>
<p>This is badsort.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span style="color:blue;font-weight:bold;">import</span> <span>Data.List</span> <span style="color:red;">(</span><span>permutations</span><span style="color:red;">,</span> <span>insert</span><span style="color:red;">)</span>
<span>&gt;</span> 
<span>&gt;</span> <span>badsort</span> <span style="color:red;">::</span> <span>Ord</span> <span>a</span> <span style="color:red;">=&gt;</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span style="color:red;">[</span><span>a</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span style="color:red;">[</span><span>a</span><span style="color:red;">]</span>
<span>&gt;</span> <span>badsort</span> <span class="hs-num">0</span> <span style="color:red;">=</span> <span>foldr</span> <span>insert</span> <span>[]</span>
<span>&gt;</span> <span>badsort</span> <span>k</span> <span style="color:red;">=</span> <span>head</span> <span>.</span> <span>badsort</span> <span style="color:red;">(</span><span>k</span><span style="color:green;">-</span><span class="hs-num">1</span><span style="color:red;">)</span> <span>.</span> <span>permutations</span>
</code></pre>
<p>Claim: <code>badsort k</code> is a correct sorting algorithm for any natural number <code>k</code>. Before reading further I recommend staring at that until you understand what it’s doing and why it works.</p>
<h2 id="how-badsort-works">How badsort works</h2>
<p>Badsort is very bad. Here’s how it works:</p>
<ul>
<li><p><code>badsort 0</code> is just plain old insertion sort.</p></li>
<li><p><code>badsort k xs</code> creates the list of all permutations of <code>xs</code>, sorts them into lexicographic order, and selects the first. This works because the lexicographically smallest permutation of <code>xs</code> is, in fact, the one which is sorted.</p>
<p>Oh, and of course, sorting the permutations lexicographically is done by a recursive call to <code>badsort (k-1)</code>. (As an aside, I like how seamless this is in Haskell with polymorphic recursion—each recursive call is at a different type.)</p></li>
</ul>
<p>Here are a few examples to show that it works:</p>
<pre><code><span style="color:gray;">ghci&gt; </span>badsort 0 [3,1,2]
  [1,2,3]

<span style="color:gray;">ghci&gt; </span>badsort 1 [3,1,2]  -- generates 6 permutations
  [1,2,3]

<span style="color:gray;">ghci&gt; </span>badsort 2 [3,1,2]  -- generates 720 permutations of 6 permutations
  [1,2,3]
</code></pre>
<p><code>badsort 3 [3,1,2]</code>, if we tried it (not recommended!!), would generate all possible permutations of the list of 720 permutations of the list of 6 permutations of <code>[3,1,2]</code>. The number of such permutations is, of course, $720!$, which has $1747$ decimal digits; there is literally not enough space in the universe to store all those permutations.</p>
<p>In general, <code>badsort k</code> is a correct sorting algorithm<a href="#fn1" class="footnote-ref" id="fnref1"><sup>1</sup></a> which takes time $\Omega((n!^{(k)})^2)$, where $n!^{(k)}$ denotes the $k$-fold iterated factorial of $n$, that is, $n!^{(0)} = n$ and $n!^{(k+1)} = (n!^{(k)})!$. (This doesn’t even take into account the time for accessing memory; at this scale we certainly <a href="http://www.ilikebigbits.com/2014_04_21_myth_of_ram_1.html">can’t assume memory access takes constant time</a>. Fetching memory from a data center in another galaxy takes a while, you know? =)</p>
<h2 id="it-gets-worse">It gets worse</h2>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>worstsort</span> <span style="color:red;">::</span> <span>Ord</span> <span>a</span> <span style="color:red;">=&gt;</span> <span style="color:red;">(</span><span>Integer</span> <span style="color:red;">-&gt;</span> <span>Integer</span><span style="color:red;">)</span> <span style="color:red;">-&gt;</span> <span style="color:red;">[</span><span>a</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span style="color:red;">[</span><span>a</span><span style="color:red;">]</span>
<span>&gt;</span> <span>worstsort</span> <span>f</span> <span>xs</span> <span style="color:red;">=</span> <span>badsort</span> <span style="color:red;">(</span><span>f</span> <span>n</span><span style="color:red;">)</span> <span>xs</span>
<span>&gt;</span>   <span style="color:blue;font-weight:bold;">where</span>
<span>&gt;</span>     <span>n</span> <span style="color:red;">=</span> <span>fromIntegral</span> <span>$</span> <span>length</span> <span>xs</span>
</code></pre>
<p>Worstsort is parameterized by a function on natural numbers, and calls <code>badsort</code> with a recursion depth given by the function $f$ applied to the length of the list. Oh my.</p>
<p>Just for fun, let’s try, oh, say, the Ackermann function.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>&gt;</span> <span>ack</span> <span style="color:red;">::</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span>Integer</span>
<span>&gt;</span> <span>ack</span> <span class="hs-num">0</span> <span>n</span> <span style="color:red;">=</span> <span>n</span><span>+</span><span class="hs-num">1</span>
<span>&gt;</span> <span>ack</span> <span>m</span> <span class="hs-num">0</span> <span style="color:red;">=</span> <span>ack</span> <span style="color:red;">(</span><span>m</span><span style="color:green;">-</span><span class="hs-num">1</span><span style="color:red;">)</span> <span class="hs-num">1</span>
<span>&gt;</span> <span>ack</span> <span>m</span> <span>n</span> <span style="color:red;">=</span> <span>ack</span> <span style="color:red;">(</span><span>m</span><span style="color:green;">-</span><span class="hs-num">1</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>ack</span> <span>m</span> <span style="color:red;">(</span><span>n</span><span style="color:green;">-</span><span class="hs-num">1</span><span style="color:red;">)</span><span style="color:red;">)</span>
<span>&gt;</span> 
<span>&gt;</span> <span>diabolicalsort</span> <span style="color:red;">::</span> <span>Ord</span> <span>a</span> <span style="color:red;">=&gt;</span> <span style="color:red;">[</span><span>a</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span style="color:red;">[</span><span>a</span><span style="color:red;">]</span>
<span>&gt;</span> <span>diabolicalsort</span> <span style="color:red;">=</span> <span>worstsort</span> <span style="color:red;">(</span><span style="color:red;">\</span><span>n</span> <span style="color:red;">-&gt;</span> <span>ack</span> <span>n</span> <span>n</span><span style="color:red;">)</span>
</code></pre>
<p>Here are some fun properties of <code>diabolicalsort</code> (and any other instantiation of <code>worstsort</code>):</p>
<ul>
<li><p>It will provably terminate in a finite amount of time for any input! Although probably the words “terminate” and “finite” should be in scare quotes.</p></li>
<li><p>In some sense I can’t quite define formally but still believe in my heart, it “doesn’t cheat” in the sense that it is always “making real progress” towards sorting the input list. If you are trying to design a slow sorting algorithm, it would be cheating, for example, to make an algorithm that spins in a useless loop for a thousand years and then does insertion sort.</p></li>
<li><p>It works in practice on lists of length 1 or 2, but length 3 is completely hopeless. <code>ack 3 3 = 61</code>, so we are looking at the 61-fold iterated factorial of 3, which is a… rather large number.</p></li>
<li><p><code>ack 4 4</code> is $2^{2^{2^{65536}}} - 3$; there are not enough atoms in the universe to even <em>write down</em> this number in base 10. And <em>then</em> of course we take that number and iterate factorial that many times on $4$. Sheesh.</p></li>
<li><p>Let us not even speak of lists of length 5.</p></li>
</ul>
<p>The upshot of this, in the end, is that it is possible to make a “non-cheating” sorting algorithm whose running time grows faster than any computable function you care to choose (proof: take your chosen computable function and substitute it for <code>f</code>).</p>
<section class="footnotes">
<hr />
<ol>
<li id="fn1"><p>It might be a fun exercise to prove this formally using a proof assistant.<a href="#fnref1" class="footnote-back">↩</a></p></li>
</ol>
</section>

