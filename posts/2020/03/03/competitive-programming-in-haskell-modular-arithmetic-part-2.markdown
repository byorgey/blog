---
katex: true
title: 'Competitive Programming in Haskell: modular arithmetic, part 2'
published: 2020-03-03T11:57:25Z
categories: haskell
tags: competitive,number,programming,theory
---

<p>In <a href="https://byorgey.github.io/blog/posts/2020/02/15/competitive-programming-in-haskell-modular-arithmetic-part-1.html">my last post</a> I wrote about modular exponentiation and <code>egcd</code>. In this post, I consider the problem of solving modular equivalences, building on code from the previous post.</p>
<h1 id="solving-linear-congruences">Solving linear congruences</h1>
<p>A <em>linear congruence</em> is a modular equivalence of the form</p>
<div style="text-align:center;">
<p>$ax \equiv b \pmod m$.</p>
</div>
<p>Let’s write a function to solve such equivalences for $x$. We want a pair of integers $y$ and $k$ such that $x$ is a solution to $ax \equiv b \pmod m$ if and only if $x \equiv y \pmod k$. This isn’t hard to write in the end, but takes a little bit of thought to do it properly.</p>
<p>First of all, if $a$ and $m$ are relatively prime (that is, $\gcd(a,m) = 1$) then we know from the last post that $a$ has an inverse modulo $m$; multiplying both sides by $a^{-1}$ yields the solution $x \equiv a^{-1} b \pmod m$.</p>
<p>OK, but what if $\gcd(a,m) &gt; 1$? In this case there might not even be any solutions. For example, $2x \equiv 3 \pmod 4$ has no solutions: any even number will be equivalent to $0$ or $2$ modulo $4$, so there is no value of $x$ such that double it will be equivalent to $3$. On the other hand, $2x \equiv 2 \pmod 4$ is OK: this will be true for any odd value of $x$, that is, $x \equiv 1 \pmod 2$. In fact, it is easy to see that any common divisor of $a$ and $m$ must also divide $b$ in order to have any solutions. In case the GCD of $a$ and $m$ does divide $b$, we can simply divide through by the GCD (<em>including</em> dividing the modulus $m$!) and then solve the resulting equivalence.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:green;">-- solveMod a b m solves ax = b (mod m), returning a pair (y,k) (with</span>
<span style="color:green;">-- 0 &lt;= y &lt; k) such that x is a solution iff x = y (mod k).</span>
<span>solveMod</span> <span style="color:red;">::</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span>Integer</span> <span style="color:red;">-&gt;</span> <span>Maybe</span> <span style="color:red;">(</span><span>Integer</span><span style="color:red;">,</span> <span>Integer</span><span style="color:red;">)</span>
<span>solveMod</span> <span>a</span> <span>b</span> <span>m</span>
  <span style="color:red;">|</span> <span>g</span> <span>==</span> <span class="hs-num">1</span>         <span style="color:red;">=</span> <span>Just</span> <span style="color:red;">(</span><span style="color:red;">(</span><span>b</span> <span>*</span> <span>inverse</span> <span>m</span> <span>a</span><span style="color:red;">)</span> <span>`mod`</span> <span>m</span><span style="color:red;">,</span> <span>m</span><span style="color:red;">)</span>
  <span style="color:red;">|</span> <span>b</span> <span>`mod`</span> <span>g</span> <span>==</span> <span class="hs-num">0</span> <span style="color:red;">=</span> <span>solveMod</span> <span style="color:red;">(</span><span>a</span> <span>`div`</span> <span>g</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>b</span> <span>`div`</span> <span>g</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>m</span> <span>`div`</span> <span>g</span><span style="color:red;">)</span>
  <span style="color:red;">|</span> <span>otherwise</span>      <span style="color:red;">=</span> <span>Nothing</span>
  <span style="color:blue;font-weight:bold;">where</span>
    <span>g</span> <span style="color:red;">=</span> <span>gcd</span> <span>a</span> <span>m</span></code></pre>
<h1 id="solving-systems-of-congruences-with-crt">Solving systems of congruences with CRT</h1>
<p>In its most basic form, the <a href="https://mathlesstraveled.com/2019/04/05/more-words-about-pww-25-the-chinese-remainder-theorem/"><em>Chinese remainder theorem</em> (CRT)</a> says that if we have a system of two modular equations</p>
<p>$\begin{array}{rcl}x &\equiv& a \pmod m \\ x &\equiv& b \pmod n\end{array}$</p>
<p>then as long as $m$ and $n$ are relatively prime, there is a <em>unique</em> solution for $x$ modulo the product $mn$; that is, the system of two equations is equivalent to a single equation of the form</p>
<p>$x \equiv c \pmod {mn}.$</p>
<p>We first compute the Bézout coefficients $u$ and $v$ such that $mu + nv = 1$ <a href="https://byorgey.github.io/blog/posts/2020/02/15/competitive-programming-in-haskell-modular-arithmetic-part-1.html">using <code>egcd</code></a>, and then compute the solution as $c = anv + bmu$. Indeed,</p>
<p>$c = anv + bmu = a(1 - mu) + bmu = a - amu + bmu = a + (b-a)mu$</p>
<p>and hence $c \equiv a \pmod m$; similarly $c \equiv b \pmod n$.</p>
<p>However, this is not quite general enough: we want to still be able to say something useful even if $\gcd(m,n) &gt; 1$. I won’t go through the whole proof, but it turns out that there is a solution if and only if $a \equiv b \pmod {\gcd(m,n)}$, and we can just divide everything through by $g = \gcd(m,n)$, as we did for solving linear congruences. Here’s the code:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:green;">-- gcrt2 (a,n) (b,m) solves the pair of modular equations</span>
<span style="color:green;">--</span>
<span style="color:green;">--   x = a (mod n)</span>
<span style="color:green;">--   x = b (mod m)</span>
<span style="color:green;">--</span>
<span style="color:green;">-- It returns a pair (c, k) such that all solutions for x satisfy x =</span>
<span style="color:green;">-- c (mod k), that is, solutions are of the form x = kt + c for</span>
<span style="color:green;">-- integer t.</span>
<span>gcrt2</span> <span style="color:red;">::</span> <span style="color:red;">(</span><span>Integer</span><span style="color:red;">,</span> <span>Integer</span><span style="color:red;">)</span> <span style="color:red;">-&gt;</span> <span style="color:red;">(</span><span>Integer</span><span style="color:red;">,</span> <span>Integer</span><span style="color:red;">)</span> <span style="color:red;">-&gt;</span> <span>Maybe</span> <span style="color:red;">(</span><span>Integer</span><span style="color:red;">,</span> <span>Integer</span><span style="color:red;">)</span>
<span>gcrt2</span> <span style="color:red;">(</span><span>a</span><span style="color:red;">,</span><span>n</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>b</span><span style="color:red;">,</span><span>m</span><span style="color:red;">)</span>
  <span style="color:red;">|</span> <span>a</span> <span>`mod`</span> <span>g</span> <span>==</span> <span>b</span> <span>`mod`</span> <span>g</span> <span style="color:red;">=</span> <span>Just</span> <span style="color:red;">(</span><span style="color:red;">(</span><span style="color:red;">(</span><span>a</span><span>*</span><span>v</span><span>*</span><span>m</span> <span>+</span> <span>b</span><span>*</span><span>u</span><span>*</span><span>n</span><span style="color:red;">)</span> <span>`div`</span> <span>g</span><span style="color:red;">)</span> <span>`mod`</span> <span>k</span><span style="color:red;">,</span> <span>k</span><span style="color:red;">)</span>
  <span style="color:red;">|</span> <span>otherwise</span>              <span style="color:red;">=</span> <span>Nothing</span>
  <span style="color:blue;font-weight:bold;">where</span>
    <span style="color:red;">(</span><span>g</span><span style="color:red;">,</span><span>u</span><span style="color:red;">,</span><span>v</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>egcd</span> <span>n</span> <span>m</span>
    <span>k</span> <span style="color:red;">=</span> <span style="color:red;">(</span><span>m</span><span>*</span><span>n</span><span style="color:red;">)</span> <span>`div`</span> <span>g</span></code></pre>
<p>From here we can bootstrap ourselves into solving systems of more than two equations, by iteratively combining two equations into one.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:green;">-- gcrt solves a system of modular equations.  Each equation x = a</span>
<span style="color:green;">-- (mod n) is given as a pair (a,n).  Returns a pair (z, k) such that</span>
<span style="color:green;">-- solutions for x satisfy x = z (mod k), that is, solutions are of</span>
<span style="color:green;">-- the form x = kt + z for integer t.</span>
<span>gcrt</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span style="color:red;">(</span><span>Integer</span><span style="color:red;">,</span> <span>Integer</span><span style="color:red;">)</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>Maybe</span> <span style="color:red;">(</span><span>Integer</span><span style="color:red;">,</span> <span>Integer</span><span style="color:red;">)</span>
<span>gcrt</span> <span>[]</span>         <span style="color:red;">=</span> <span>Nothing</span>
<span>gcrt</span> <span style="color:red;">[</span><span>e</span><span style="color:red;">]</span>        <span style="color:red;">=</span> <span>Just</span> <span>e</span>
<span>gcrt</span> <span style="color:red;">(</span><span>e1</span><span>:</span><span>e2</span><span>:</span><span>es</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>gcrt2</span> <span>e1</span> <span>e2</span> <span>&gt;&gt;=</span> <span style="color:red;">\</span><span>e</span> <span style="color:red;">-&gt;</span> <span>gcrt</span> <span style="color:red;">(</span><span>e</span><span>:</span><span>es</span><span style="color:red;">)</span></code></pre>
<h1 id="practice-problems">Practice problems</h1>
<p>And here are a bunch of problems for you to practice!</p>
<ul>
<li><a href="http://open.kattis.com/problems/chineseremainder">Chinese Remainder</a></li>
<li><a href="http://open.kattis.com/problems/generalchineseremainder">Chinese Remainder Theorem (non-relatively prime moduli)</a></li>
<li><a href="http://open.kattis.com/problems/disgruntledjudge">Disgruntled Judge</a></li>
<li><a href="http://open.kattis.com/problems/astro">Astro</a></li>
<li><a href="http://open.kattis.com/problems/heliocentric">Heliocentric</a></li>
<li><a href="http://open.kattis.com/problems/remainderreminder">Remainder Reminder</a></li>
<li><a href="http://open.kattis.com/problems/dvdscreensaver">DVD Screensaver</a></li>
</ul>

