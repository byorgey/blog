---
title: 'Competitive Programming in Haskell: primes and factoring'
published: 2020-02-07T22:21:13Z
categories: haskell
tags: competitive,number,programming,theory
---

<p>Number theory is a topic that comes up fairly regularly in competitive programming, and it’s a very nice fit for Haskell. I’ve developed a bunch of code over the years that regularly comes in handy. None of this is particularly optimized, and it’s definitely no match for a specialized library like <a href="https://hackage.haskell.org/package/arithmoi">arithmoi</a>, but in a competitive programming context it usually does the trick!</p>
<p>A few imports first:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:blue;font-weight:bold;">import</span>           <span>Control.Arrow</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Data.List</span>     <span style="color:red;">(</span><span>group</span><span style="color:red;">,</span> <span>sort</span><span style="color:red;">)</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Data.Map</span>      <span style="color:red;">(</span><span>Map</span><span style="color:red;">)</span>
<span style="color:blue;font-weight:bold;">import</span> <span style="color:blue;font-weight:bold;">qualified</span> <span>Data.Map</span>      <span style="color:blue;font-weight:bold;">as</span> <span>M</span></code></pre>
<h1 id="primes">Primes</h1>
<p>We start with a basic definition of the list of primes, made with a simple recursive sieve, but with one very big optimization: when we find a prime $p$, instead of simply filtering out all the multiples of $p$ in the rest of the list, we first take all the numbers less than $p^2$ and pass them through without testing; composite numbers less than $p^2$ would have already been filtered out by a smaller prime.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>primes</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span>Integer</span><span style="color:red;">]</span>
<span>primes</span> <span style="color:red;">=</span> <span class="hs-num">2</span> <span>:</span> <span>sieve</span> <span>primes</span> <span style="color:red;">[</span><span class="hs-num">3</span><span style="color:red;">..</span><span style="color:red;">]</span>
  <span style="color:blue;font-weight:bold;">where</span>
    <span>sieve</span> <span style="color:red;">(</span><span>p</span><span>:</span><span>ps</span><span style="color:red;">)</span> <span>xs</span> <span style="color:red;">=</span>
      <span style="color:blue;font-weight:bold;">let</span> <span style="color:red;">(</span><span>h</span><span style="color:red;">,</span><span>t</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>span</span> <span style="color:red;">(</span><span>&lt;</span> <span>p</span><span>*</span><span>p</span><span style="color:red;">)</span> <span>xs</span>
      <span style="color:blue;font-weight:bold;">in</span>  <span>h</span> <span>++</span> <span>sieve</span> <span>ps</span> <span style="color:red;">(</span><span>filter</span> <span style="color:red;">(</span><span style="color:red;">(</span><span>/=</span><span class="hs-num">0</span><span style="color:red;">)</span><span>.</span><span style="color:red;">(</span><span>`mod`</span><span>p</span><span style="color:red;">)</span><span style="color:red;">)</span> <span>t</span><span style="color:red;">)</span></code></pre>
<p>I got this code from the <a href="https://wiki.haskell.org/Prime_numbers">Haskell wiki page on prime numbers</a>. On my machine this allows us to find all the primes up to one million in about 4 seconds. Not blazing fast by any means, and of course this is <a href="https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf">not actually a true sieve</a>—but it’s short, relatively easy to remember, and works just fine for many purposes. (There are some competitive programming problems requiring a true sieve, but I typically solve those in Java. Maybe someday I will figure out a concise way to solve them in Haskell.)</p>
<h1 id="factoring">Factoring</h1>
<p>Now that we have our list of primes, we can write a function to find prime factorizations:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>listFactors</span> <span style="color:red;">::</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span style="color:red;">[</span><span>Integer</span><span style="color:red;">]</span>
<span>listFactors</span> <span style="color:red;">=</span> <span>go</span> <span>primes</span>
  <span style="color:blue;font-weight:bold;">where</span>
    <span>go</span> <span style="color:blue;font-weight:bold;">_</span>      <span class="hs-num">1</span> <span style="color:red;">=</span> <span>[]</span>
    <span>go</span> <span style="color:red;">(</span><span>p</span><span>:</span><span>ps</span><span style="color:red;">)</span> <span>n</span>
      <span style="color:red;">|</span> <span>p</span><span>*</span><span>p</span> <span>&gt;</span> <span>n</span> <span style="color:red;">=</span> <span style="color:red;">[</span><span>n</span><span style="color:red;">]</span>
      <span style="color:red;">|</span> <span>n</span> <span>`mod`</span> <span>p</span> <span>==</span> <span class="hs-num">0</span> <span style="color:red;">=</span> <span>p</span> <span>:</span> <span>go</span> <span style="color:red;">(</span><span>p</span><span>:</span><span>ps</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>n</span> <span>`div`</span> <span>p</span><span style="color:red;">)</span>
      <span style="color:red;">|</span> <span>otherwise</span>      <span style="color:red;">=</span> <span>go</span> <span>ps</span> <span>n</span></code></pre>
<p>This is relatively straightforward. Note how we stop when the next prime is greater than the square root of the number being tested, because if there were a prime factor we would have already found it by that point.</p>
<h1 id="and-related-functions">…and related functions</h1>
<p>Finally we can use <code>listFactors</code> to build a few other useful functions:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>factor</span> <span style="color:red;">::</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span>Map</span> <span>Integer</span> <span>Int</span>
<span>factor</span> <span style="color:red;">=</span> <span>listFactors</span> <span>&gt;&gt;&gt;</span> <span>group</span> <span>&gt;&gt;&gt;</span> <span>map</span> <span style="color:red;">(</span><span>head</span> <span>&amp;&amp;&amp;</span> <span>length</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span> <span>M.fromList</span>

<span>divisors</span> <span style="color:red;">::</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span style="color:red;">[</span><span>Integer</span><span style="color:red;">]</span>
<span>divisors</span> <span style="color:red;">=</span> <span>factor</span> <span>&gt;&gt;&gt;</span> <span>M.assocs</span> <span>&gt;&gt;&gt;</span> <span>map</span> <span style="color:red;">(</span><span style="color:red;">\</span><span style="color:red;">(</span><span>p</span><span style="color:red;">,</span><span>k</span><span style="color:red;">)</span> <span style="color:red;">-&gt;</span> <span>take</span> <span style="color:red;">(</span><span>k</span><span>+</span><span class="hs-num">1</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>iterate</span> <span style="color:red;">(</span><span>*</span><span>p</span><span style="color:red;">)</span> <span class="hs-num">1</span><span style="color:red;">)</span><span style="color:red;">)</span>
  <span>&gt;&gt;&gt;</span> <span>sequence</span> <span>&gt;&gt;&gt;</span> <span>map</span> <span>product</span>

<span>totient</span> <span style="color:red;">::</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span>Integer</span>
<span>totient</span> <span style="color:red;">=</span> <span>factor</span> <span>&gt;&gt;&gt;</span> <span>M.assocs</span> <span>&gt;&gt;&gt;</span> <span>map</span> <span style="color:red;">(</span><span style="color:red;">\</span><span style="color:red;">(</span><span>p</span><span style="color:red;">,</span><span>k</span><span style="color:red;">)</span> <span style="color:red;">-&gt;</span> <span>p</span><span>^</span><span style="color:red;">(</span><span>k</span><span style="color:green;">-</span><span class="hs-num">1</span><span style="color:red;">)</span> <span>*</span> <span style="color:red;">(</span><span>p</span><span style="color:green;">-</span><span class="hs-num">1</span><span style="color:red;">)</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span> <span>product</span></code></pre>
<p><code>factor</code> yields a <code>Map</code> whose keys are unique primes and whose values are the corresponding powers; for example, <code>factor 600 = M.fromList [(2,3), (3,1), (5,2)]</code>, corresponding to the factorization $600 = 2^3 \cdot 3 \cdot 5^2$. It works by grouping together like prime factors (note that <code>listFactors</code> guarantees to generate a sorted list of prime factors), counting each group, and building a <code>Map</code>.</p>
<p><code>divisors n</code> generates a list of all divisors of <code>n</code>. It works by generating all powers of each prime from $p^0$ up to $p^k$, and combining them in all possible ways using <code>sequence</code>. Note it does not guarantee to generate the divisors in order.</p>
<p><code>totient</code> implements the <a href="https://en.wikipedia.org/wiki/Euler%20totient%20function">Euler totient function</a>: <code>totient n</code> says how many numbers from <code>1</code> to <code>n</code> are relatively prime to <code>n</code>. To understand how it works, see this series of four blog posts I wrote on my other blog: <a href="https://mathlesstraveled.com/2019/05/09/computing-the-euler-totient-function-part-1/">part 1</a>, <a href="https://mathlesstraveled.com/2019/05/18/computing-the-euler-totient-function-part-2-seeing-phi-is-multiplicative/">part 2</a>, <a href="https://mathlesstraveled.com/2019/05/27/computing-the-euler-totient-function-part-3-proving-phi-is-multiplicative/">part 3</a>, <a href="https://mathlesstraveled.com/2019/07/02/computing-the-euler-totient-function-part-4-totient-of-prime-powers/">part 4</a>.</p>
<h1 id="problems">Problems</h1>
<p>Here are a few problems for you to try (ordered roughly from easier to more difficult):</p>
<ul>
<li><a href="https://open.kattis.com/problems/multigram">multigram</a></li>
<li><a href="https://open.kattis.com/problems/relatives">relatives</a></li>
<li><a href="https://open.kattis.com/problems/magical3">magical3</a></li>
<li><a href="https://open.kattis.com/problems/perfectpowers">perfectpowers</a></li>
<li><a href="https://open.kattis.com/problems/factovisors">factovisors</a></li>
</ul>
<p>In a subsequent post I’ll continue on the number theory theme and talk about modular arithmetic.</p>

