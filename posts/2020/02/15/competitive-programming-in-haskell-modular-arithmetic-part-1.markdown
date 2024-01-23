---
title: 'Competitive Programming in Haskell: modular arithmetic, part 1'
published: 2020-02-15T21:06:10Z
categories: haskell
tags: competitive,number,programming,theory
---

<p>Modular arithmetic comes up a lot in computer science, and so it’s no surprise that it is featured, either explicitly or implicitly, in many competitive programming problems.</p>
<p>As a brief aside, to be good at competitive programming it’s not enough to have a library of code at your disposal (though it certainly helps!). You must also deeply understand the code in your library and how it works—so that you know when it is applicable, what the potential pitfalls are, how to debug when things don’t work, and how to make modifications to the code to fit some new problem. I will try to explain all the code I exhibit here—why, and not just how, it works. But you’ll also ultimately be better off if you write your own code rather than using mine! Read my explanations for ideas, and then go see if you can replicate the functionality you need.</p>
<h1 id="modular-exponentiation">Modular exponentiation</h1>
<p>We start with a simple implementation of <em>modular exponentiation</em>, that is, computing $b^e \pmod m$, via <a href="https://en.wikipedia.org/wiki/Exponentiation_by_squaring">repeated squaring</a>. This comes up occasionally in both number theory problems (unsurprisingly) and combinatorics problems (because such problems often ask for a very large answer to be given modulo $10^9+7$ or some other large prime).</p>
<p>This works via the recurrence</p>
<p>$\begin{array}{rcl}x^0 &amp;=&amp; 1 \\[0.5em] x^{2n} &amp;=&amp; (x^n)^2 \\[0.5em] x^{2n+1} &amp;=&amp; x \cdot (x^n)^2\end{array}$</p>
<p>and using the fact that taking the remainder $\pmod m$ commutes with multiplication.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>modexp</span> <span style="color:red;">::</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span>Integer</span>
<span>modexp</span> <span style="color:blue;font-weight:bold;">_</span> <span class="hs-num">0</span> <span style="color:blue;font-weight:bold;">_</span> <span style="color:red;">=</span> <span class="hs-num">1</span>
<span>modexp</span> <span>b</span> <span>e</span> <span>m</span>
  <span style="color:red;">|</span> <span>even</span> <span>e</span>    <span style="color:red;">=</span> <span style="color:red;">(</span><span>r</span><span>*</span><span>r</span><span style="color:red;">)</span> <span>`mod`</span> <span>m</span>
  <span style="color:red;">|</span> <span>otherwise</span> <span style="color:red;">=</span> <span style="color:red;">(</span><span>b</span><span>*</span><span>r</span><span>*</span><span>r</span><span style="color:red;">)</span> <span>`mod`</span> <span>m</span>
  <span style="color:blue;font-weight:bold;">where</span>
    <span>r</span> <span style="color:red;">=</span> <span>modexp</span> <span>b</span> <span style="color:red;">(</span><span>e</span> <span>`div`</span> <span class="hs-num">2</span><span style="color:red;">)</span> <span>m</span></code></pre>
<p>This could probably be slightly optimized, but it’s hardly worth it; since the number of multiplications performed is proportional to the logarithm of the exponent, it’s pretty much instantaneous for any inputs that would be used in practice.</p>
<p>However, there’s another technique, obvious in retrospect, that I have recently discovered. Many competitive programming problems ask you to compute the answer modulo some fixed number (usually a large prime). In this context, all arithmetic operations are going to be carried out modulo the same value. With Haskell’s great facilities for cheap abstraction it makes perfect sense to write something like this:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>m</span> <span style="color:red;">::</span> <span>Integer</span>
<span>m</span> <span style="color:red;">=</span> <span class="hs-num">10</span><span>^</span><span class="hs-num">9</span> <span>+</span> <span class="hs-num">7</span>   <span style="color:green;">-- or whatever the modulus is supposed to be</span>

<span style="color:green;">-- Make a newtype for integers mod m</span>
<span style="color:blue;font-weight:bold;">newtype</span> <span>M</span> <span style="color:red;">=</span> <span>M</span> <span style="color:red;">{</span> <span>unM</span> <span style="color:red;">::</span> <span>Integer</span> <span style="color:red;">}</span>
  <span style="color:blue;font-weight:bold;">deriving</span> <span style="color:red;">(</span><span>Eq</span><span style="color:red;">,</span> <span>Ord</span><span style="color:red;">)</span>
<span style="color:blue;font-weight:bold;">instance</span> <span>Show</span> <span>M</span> <span style="color:blue;font-weight:bold;">where</span> <span>show</span> <span style="color:red;">=</span> <span>show</span> <span>.</span> <span>unM</span>

<span style="color:green;">-- Do all arithmetic operations mod m</span>
<span style="color:blue;font-weight:bold;">instance</span> <span>Num</span> <span>M</span> <span style="color:blue;font-weight:bold;">where</span>
  <span>fromInteger</span> <span>n</span> <span style="color:red;">=</span> <span>M</span> <span style="color:red;">(</span><span>n</span> <span>`mod`</span> <span>m</span><span style="color:red;">)</span>
  <span style="color:red;">(</span><span>M</span> <span>a</span><span style="color:red;">)</span> <span>+</span> <span style="color:red;">(</span><span>M</span> <span>b</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>M</span> <span style="color:red;">(</span><span style="color:red;">(</span><span>a</span> <span>+</span> <span>b</span><span style="color:red;">)</span> <span>`mod`</span> <span>m</span><span style="color:red;">)</span>
  <span style="color:red;">(</span><span>M</span> <span>a</span><span style="color:red;">)</span> <span style="color:green;">-</span> <span style="color:red;">(</span><span>M</span> <span>b</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>M</span> <span style="color:red;">(</span><span style="color:red;">(</span><span>a</span> <span style="color:green;">-</span> <span>b</span><span style="color:red;">)</span> <span>`mod`</span> <span>m</span><span style="color:red;">)</span>
  <span style="color:red;">(</span><span>M</span> <span>a</span><span style="color:red;">)</span> <span>*</span> <span style="color:red;">(</span><span>M</span> <span>b</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>M</span> <span style="color:red;">(</span><span style="color:red;">(</span><span>a</span> <span>*</span> <span>b</span><span style="color:red;">)</span> <span>`mod`</span> <span>m</span><span style="color:red;">)</span>
  <span>abs</span>    <span style="color:red;">=</span> <span>undefined</span>  <span style="color:green;">-- make the warnings stop</span>
  <span>signum</span> <span style="color:red;">=</span> <span>undefined</span></code></pre>
<p>The fun thing is that now the normal exponentiation operator <code>(^)</code> does modular exponentiation for free! It is implemented using repeated squaring so it’s quite efficient. You can now write all your code using the <code>M</code> type with normal arithmetic operations, and it will all be carried out mod <code>m</code> automatically.</p>
<p>Here are a couple problems for you to try:</p>
<ul>
<li><a href="https://open.kattis.com/problems/powers">Powers and Modulus</a></li>
<li><a href="https://open.kattis.com/problems/abstractpainting">Abstract Painting</a></li>
</ul>
<h1 id="extended-gcd">Extended gcd</h1>
<p>Beyond modular exponentiation, the workhorse of many number theory problems is the <em>extended Euclidean Algorithm</em>. It not only computes the GCD $g$ of $a$ and $b$, but also computes $x$ and $y$ such that $ax + by = g$ (which are guaranteed to exist by <a href="https://mathlesstraveled.com/2015/11/25/mablowrimo-24-bezouts-identity/">Bezout’s identity</a>).</p>
<p>First, let’s recall how to compute the GCD via <a href="https://mathlesstraveled.com/2008/02/11/recounting-the-rationals-part-ivb-the-euclidean-algorithm/">Euclid’s Algorithm</a>:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>gcd</span> <span>a</span> <span class="hs-num">0</span> <span style="color:red;">=</span> <span>abs</span> <span>a</span>
<span>gcd</span> <span>a</span> <span>b</span> <span style="color:red;">=</span> <span>gcd</span> <span>b</span> <span style="color:red;">(</span><span>a</span> <span>`mod`</span> <span>b</span><span style="color:red;">)</span></code></pre>
<p>I won’t explain how this works here; you can go read about it at the link above, and it is well-covered elsewhere. But let’s think how we would find appropriate values $x$ and $y$ at the same time. Suppose the recursive call <code>gcd b (a `mod` b)</code>, in addition to returning the greatest common divisor $g$, were to also return values $x$ and $y$ such that $bx + (a \bmod b)y = g$. Then our goal is to find $x'$ and $y'$ such that $ax' + by' = g$, which we can compute as follows:</p>
<p>$\begin{array}{rcl}g &amp;=&amp; bx + (a \bmod b)y \\[0.5em] &amp;=&amp; bx + (a - b\lfloor a/b \rfloor)y \\[0.5em] &amp;=&amp; bx + ay - b\lfloor a/b \rfloor y = ay + b(x - \lfloor a/b \rfloor y)\end{array}$</p>
<p>Hence $x' = y$ and $y' = x - \lfloor a/b \rfloor y$. Note the key step of writing $a \bmod b = a - b \lfloor a/b \rfloor$: If we take the integer quotient of $a$ divided by $b$ and then multiply by $b$ again, we don’t necessarily get $a$ back exactly, but what we do get is the next smaller multiple of $b$. Subtracting this from the original $a$ gives $a \bmod b$.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:green;">-- egcd a b = (g,x,y)</span>
<span style="color:green;">--   g is the gcd of a and b, and ax + by = g</span>
<span>egcd</span> <span style="color:red;">::</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span style="color:red;">(</span><span>Integer</span><span style="color:red;">,</span> <span>Integer</span><span style="color:red;">,</span> <span>Integer</span><span style="color:red;">)</span>
<span>egcd</span> <span>a</span> <span class="hs-num">0</span> <span style="color:red;">=</span> <span style="color:red;">(</span><span>abs</span> <span>a</span><span style="color:red;">,</span> <span>signum</span> <span>a</span><span style="color:red;">,</span> <span class="hs-num">0</span><span style="color:red;">)</span>
<span>egcd</span> <span>a</span> <span>b</span> <span style="color:red;">=</span> <span style="color:red;">(</span><span>g</span><span style="color:red;">,</span> <span>y</span><span style="color:red;">,</span> <span>x</span> <span style="color:green;">-</span> <span style="color:red;">(</span><span>a</span> <span>`div`</span> <span>b</span><span style="color:red;">)</span> <span>*</span> <span>y</span><span style="color:red;">)</span>
  <span style="color:blue;font-weight:bold;">where</span>
    <span style="color:red;">(</span><span>g</span><span style="color:red;">,</span><span>x</span><span style="color:red;">,</span><span>y</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>egcd</span> <span>b</span> <span style="color:red;">(</span><span>a</span> <span>`mod`</span> <span>b</span><span style="color:red;">)</span></code></pre>
<p>Finally, <code>egcd</code> allows us to find <em>modular inverses</em>. The modular inverse of $a \pmod m$ is a number $b$ such that $ab \equiv 1 \pmod m$, which will exist as long as $\gcd(m,a) = 1$: in that case, by Bezout’s identity, there exist $x$ and $y$ such that $mx + ay = 1$, and hence $mx + ay \equiv 0 + ay \equiv ay \equiv 1 \pmod m$ (since $mx \equiv 0 \pmod m$). So $y$ is the desired modular inverse of $a$.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:green;">-- inverse m a  is the multiplicative inverse of a mod m.</span>
<span>inverse</span> <span style="color:red;">::</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span>Integer</span>
<span>inverse</span> <span>m</span> <span>a</span> <span style="color:red;">=</span> <span>y</span> <span>`mod`</span> <span>m</span>
  <span style="color:blue;font-weight:bold;">where</span>
    <span style="color:red;">(</span><span style="color:blue;font-weight:bold;">_</span><span style="color:red;">,</span><span style="color:blue;font-weight:bold;">_</span><span style="color:red;">,</span><span>y</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>egcd</span> <span>m</span> <span>a</span></code></pre>
<p>Of course, this assumes that $m$ and $a$ are relatively prime; if not it will silently give a bogus answer. If you’re concerned about that you could check that the returned GCD is 1 and throw an error otherwise.</p>
<p>And here are a few problems for you to try!</p>
<ul>
<li><a href="https://open.kattis.com/problems/crackingrsa">Cracking RSA</a></li>
<li><a href="https://open.kattis.com/problems/modulararithmetic">Modular Arithmetic</a></li>
<li><a href="https://open.kattis.com/problems/seti">SETI</a></li>
<li><a href="https://open.kattis.com/problems/kingscolors">King’s Colors</a></li>
</ul>
<p>In part 2 I’ll consider the task of solving modular equations.</p>

