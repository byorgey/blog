---
title: 'Competitive programming in Haskell: Infinite 2D array, Levels 2 and 3'
published: 2023-01-16T15:09:33Z
categories: competitive programming,haskell
tags: Kattis,number
---

<p>In a <a href="https://byorgey.wordpress.com/2022/09/01/competitive-programming-in-haskell-infinite-2d-array/">previous post</a>, I challenged you to solve <a href="https://open.kattis.com/problems/infinite2darray">Infinite 2D Array</a> using Haskell. As a reminder, the problem specifies a two-parameter recurrence $F_{x,y}$, given by</p>
<ul>
<li>$F_{0,0} = 0$</li>
<li>$F_{0,1} = F_{1,0} = 1$</li>
<li>$F_{i,0} = F_{i-1,0} + F_{i-2,0}$ for $i \geq 2$</li>
<li>$F_{0,i} = F_{0,i-1} + F_{0,i-2}$ for $i \geq 2$</li>
<li>$F_{i,j} = F_{i-1,j} + F_{i,j-1}$ for $i,j \geq 1$.</li>
</ul>
<p><a href="https://byorgey.wordpress.com/2022/12/03/competitive-programming-in-haskell-infinite-2d-array-level-1/">Last time</a>, we derived a formula for $F_{x,y}$ that involves only a linear number of terms:</p>
<p>$\displaystyle F_{x,y} = \left(\sum_{1 \leq k \leq x} F_k \binom{x-k+y-1}{x-k}\right) + \left(\sum_{1 \leq k \leq y} F_k \binom{y-k+x-1}{y-k}\right) \pmod{P}$</p>
<p>While the number of terms may be linear, it can still be on the order of a million terms, so computing each term is going to have to be pretty quick in order to fit the whole thing within the one second time limit.</p>
<h2 id="fibonacci-numbers-modulo-a-prime">Fibonacci numbers modulo a prime</h2>
<p>Computing Fibonacci numbers modulo a prime is not hard, especially since we want <em>all</em> the Fibonacci numbers from 1 up to $\max(x,y)$: just compute each one by adding the previous two modulo $P$. We could also precompute a table of Fibonacci numbers mod $P$ this way. And any of the fast methods for computing individual Fibonacci numbers (for example, using <a href="https://www.haskellforall.com/2020/04/blazing-fast-fibonacci-numbers-using.html">2x2 matrix exponentiation</a>) also work just fine if you reduce everything modulo $P$ at each step, since they only involve addition, subtraction, and multiplication.</p>
<h2 id="binomial-coefficients-modulo-a-prime">Binomial coefficients modulo a prime</h2>
<p>What about binomial coefficients? Since $n$ and $k$ are not too large, and in particular since they will both be smaller than $P$, we can use the usual formula:</p>
<p>$\displaystyle \binom n k = \frac{n!}{k!(n-k)!}$</p>
<p>(If $n$ and $k$ could be much larger, or if they could be larger than $P$, we would have to use something like <a href="https://en.wikipedia.org/wiki/Lucas%27s_theorem">Lucas’s Theorem</a> or <a href="https://cp-algorithms.com/combinatorics/binomial-coefficients.html">other techniques</a>; that might make for another interesting blog post sometime.) But how do we handle division in modular arithmtic? Since we’re working modulo a prime, every value $a$ other than zero must have a <em>modular inverse</em>, that is, a value $a^{-1}$ such that $a \cdot a^{-1} \equiv 1 \pmod p$ (this is a corollary of <a href="https://mathlesstraveled.com/2015/11/25/mablowrimo-24-bezouts-identity/">Bézout’s Theorem</a>). To compute the modular inverse for a given $a$, we have a couple options. One simple way is to use <a href="https://mathlesstraveled.com/2017/10/14/four-formats-for-fermat/">Fermat’s Little Theorem</a>: if $a$ is not divisible by a prime $p$, then $a^{p-2} \cdot a = a^{p-1} \equiv 1 \pmod p$, hence $a^{p-2}$ is the modular inverse of $a$ modulo $p$, and we can compute it efficiently using repeated squaring modulo $p$. Another option is to use the <a href="https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm">extended Euclidean algorithm</a> to find the $x$ and $y$ (guaranteed to exist by Bézout’s Theorem) such that $ax + py = 1$; then $x$ is the inverse of $a$ modulo $p$.</p>
<p>Both of these methods take $O(\lg p)$ time. In my experience, computing the $p-2$ power is easier to code (especially in Haskell where we <a href="https://byorgey.wordpress.com/2020/02/15/competitive-programming-in-haskell-modular-arithmetic-part-1/">get exponentiation by repeated squaring for free</a>!), but using the extended Euclidean algorithm can be a bit faster when it’s well-optimized. (Note the extended Euclidean algorithm can be faster when $a$ is small, but raising to the $p-2$ power always takes the same number of steps no matter what $a$ is.)</p>
<h2 id="factorials-modulo-a-prime">Factorials modulo a prime</h2>
<p>Since we’re going to be repeatedly using the same factorials, one thing we absolutely must do is precompute a table of factorials mod $P$, from $0$ up to some maximum. In this case, since our formula involves things like $\binom {x-k+y-1}{x-k}$, we may need factorials up to $x + y$, so a table of size $2 \times 10^6$ will do ($x$ and $y$ can be up to $10^6$).</p>
<p>We could also precompute a table of modular inverses of factorials; to compute the inverse of $k!$, we just find the inverse of each $k$ and multiply it by the (previously computed) inverse of $(k-1)!$. (Or we could just invert the value for $k!$ stored in the other table.) Making a table of inverse factorials like this turns out not to help too much for this particular problem, but it can be an important optimization in some cases.</p>
<h2 id="the-end">The end?</h2>
<p>So we can compute each additional Fibonacci number in $O(1)$; we can also now compute binomial coefficients modulo $P$ in $O(\lg P)$, with a few $O(1)$ table lookups for factorials and an $O(\lg P)$ inversion operation. (Again, we could achieve $O(1)$ if we also stored a table of inverse factorials, but for this problem it seems the additional time needed to construct the table in the first place outweighs the time saved computing binomial coefficients.) In theory, we have everything we need to solve this problem efficiently.</p>
<p>However, for this problem, constant factors matter! There’s still quite a bit of nontrivial work I had to do to get my code fast enough. In my next and final post on this problem, we’ll walk through a few different ideas for implementing this concretely in Haskell.</p>

