---
katex: true
title: 'Crystal Ball Connection Patterns via Species and Generating Functions'
published: 2015-06-02T17:40:57Z
categories: combinatorics,haskell,math
tags: generating functions,involutions,multinomial,species,summation,telephone
---

<p>A couple weeks ago, Denise posted <a href="http://letsplaymath.net/2015/05/20/puzzle-crystal-ball-connection-patterns/">Puzzle: Crystal Ball Connection Patterns</a> on her blog, <a href="http://letsplaymath.net">Let’s Play Math</a>. I had fun playing with it and thought I would demonstrate how to apply some high-powered combinatorial techniques to it (probably not what Denise had in mind!).</p>
<p>The setup is that there are $n$ (distinct) friends who can talk to each other on the phone. Only two people can talk at a time (no conference calls). The question is to determine how many different “configurations” there are. Not everyone has to talk, so a configuration consists of some subset of the friends arranged in (unordered) conversational pairs.</p>
<p><em>Warning: spoilers ahead</em>! If you’d like to play around with this yourself (and it is indeed a nice, accessible combinatorics problem to play with), stop reading now. My goal in this post is to have fun applying some advanced tools to this (relatively) simple problem.</p>
<h1 id="telephone-numbers">Telephone numbers</h1>
<p>Let’s start by visualizing some configurations. In her post, Denise illustrated the complete set of configurations for $n = 4$, which I will visualize like this:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2015/06/80a8f658f2b293f5.png" alt="" /></p>
</div>
<p>Notice how I’ve arranged them: in the first row is the unique configuration where no one is talking (yes, that counts). In the second row are the six possible configurations with just a single conversation. The last row has the three possible configurations with two conversations.</p>
<p>One good approach at this point would be to derive some recurrences. This problem does indeed admit a nice recurrence, but I will let you ponder it. Instead, let’s see if we can just “brute-force” our way to a general formula, using our combinatorial wits. Later I will demonstrate a much more principled, mechanical way to <em>derive</em> a general formula.</p>
<p>Let’s start by coming up with a formula for $T_{n,k}$, the number of configurations with $n$ people and $k$ conversations. The number of ways of choosing $k$ pairs out of a total of $n$ is the <a href="http://en.wikipedia.org/wiki/Multinomial_theorem#Multinomial_coefficients">multinomial coefficient</a> $\displaystyle \binom{n}{2,2,\dots,2,n-2k} = \frac{n!}{(2!)^k(n-2k)!}$. However, that overcounts things: it actually distinguishes the first pair, second pair, and so on, but we don’t want to have any ordering on the pairs. So we have to divide by $k!$, the number of distinct orderings of the pairs. Thus,</p>
<p>$\displaystyle T_{n,k} = \frac{n!}{2^k (n-2k)! k!}.$</p>
<p>Let’s do a few sanity checks. First, when $k=0$, we have $T_{n,0} = \frac{n!}{n!} = 1$. We can also try some other small numbers we’ve already enumerated by hand: for example, $T_{4,1} = \frac{4!}{2 \cdot 2 \cdot 1} = 6$, and $T_{4,2} = \frac{4!}{4 \cdot 1 \cdot 2} = 3$. So this seems to work.</p>
<p>For $n$ people, there can be at most $\lfloor n/2 \rfloor$ conversations. So, the total number of configurations is going to be</p>
<p>$\displaystyle T_n = \sum_{k=0}^{\lfloor n/2 \rfloor} T_{n,k}$.</p>
<p>We can use this to compute $T_n$ for the first few values of $n$:</p>
<p>$\begin{array}{rcl}T_0 &=& 1\\T_1 &=& 1 \\ T_2 &=& 1 + 1 = 2 \\ T_3 &=& 1 + 3!/2 = 4 \\ T_4 &=& 1 + 6 + 3 = 10 \\ T_5 &=& 1 + 5!/(2 \cdot 3!) + 5!/(4 \cdot 2) = 1 + 10 + 15 = 26 \\ T_6 &=& 1 + 6!/(2 \cdot 4!) + 6!/(4 \cdot 2 \cdot 2) + 6!/(8 \cdot 3!) = 1 + 15 + 45 + 15 = 76 \end{array}$</p>
<p>At this point we could look up the <a href="https://oeis.org/A000085">sequence 1,1,2,4,10,26,76 on the OEIS</a> and find out all sorts of fun things: <em>e.g.</em> that we are also counting self-inverse permutations, <em>i.e.</em> involutions, that these numbers are also called “restricted Stirling numbers of the second kind”, some recurrence relations, <em>etc.</em>, as well as enough references to keep us busy reading for a whole year.</p>
<h1 id="species">Species</h1>
<p>We can describe configurations as elements of the <a href="https://byorgey.wordpress.com/category/species/page/2/">combinatorial species</a> $C = E \circ (E_2 + X)$. That is, a configuration is an unordered set ($E$) of ($\circ$) things ($E_2 + X$), where each thing can either be an unordered pair ($E_2$) of people talking on the phone, or ($+$) a single person ($X$) who is not talking.</p>
<p>We can now use the Haskell <a href="http://hackage.haskell.org/package/species"><code>species</code> library</a> to automatically generate some counts and see whether they agree with our manual enumerations. First, some boilerplate setup:</p>
<pre><code><span style="color:gray;">ghci&gt; </span>:set -XNoImplicitPrelude
<span style="color:gray;">ghci&gt; </span>:m +NumericPrelude
<span style="color:gray;">ghci&gt; </span>:m +Math.Combinatorics.Species</code></pre>
<p>Now we define the species of configurations:</p>
<pre><code><span style="color:gray;">ghci&gt; </span>let configurations = set `o` (set `ofSizeExactly` 2 + singleton)</code></pre>
<p>We can ask the library to count the number of configurations for different $n$:</p>
<pre><code><span style="color:gray;">ghci&gt; </span>take 10 (labelled configurations)
  [1,1,2,4,10,26,76,232,764,2620]
</code></pre>
<p>Oh good, those numbers look familiar! Now, I wonder how many configurations there are for $n = 100$?</p>
<pre><code><span style="color:gray;">ghci&gt; </span>labelled configurations !! 100
  24053347438333478953622433243028232812964119825419485684849162710512551427284402176
</code></pre>
<p>Yikes!</p>
<p>We can also use the library to generate exhaustive lists of configurations, and draw them using <a href="http://projects.haskell.org/diagrams">diagrams</a>. For example, here are all $76$ configurations for $n=6$. (If you want to see the code used to generate this diagram, you can <a href="http://hub.darcs.net/byorgey/byorgey-wordpress/browse/2015-05-telephone-numbers/telephone-numbers.markdown">find it here</a>.)</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2015/06/ab15a3034ace6d74.png" alt="" /></p>
</div>
<p>And just for fun, let’s draw all $232$ configurations for $n = 7$:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2015/06/71651d116c2cb576.png" alt="" /></p>
</div>
<p>Whee!</p>
<h1 id="a-general-formula-via-generating-functions">A general formula, via generating functions</h1>
<p>Finally, I want to show how to use the species definition given above and the theory of generating functions to (somewhat) mechanically <em>derive</em> a general formula for the number of configurations. (Hopefully it will end up being equivalent to the formula we came up with near the beginning of the post!) Of course, this is also what the <code>species</code> library is doing, but only numerically—we will do things <em>symbolically</em>.</p>
<p>First, note that we are counting labelled configurations (the friends are all distinct), so we want to consider <a href="http://blog.sigfpe.com/2007/11/small-combinatorial-library.html">exponential generating functions</a> (egfs). Recall that the egf for a species $F$ is given by</p>
<p>$\displaystyle F(x) = \sum_{n \geq 0} |F[n]| \frac{x^n}{n!}$,</p>
<p>that is, a (possibly infinite) formal power series where the coefficient of $x^n/n!$ is the number of distinct labelled $F$-structures of size $n$. In our case, we need</p>
<p>$\displaystyle E(x) = \sum_{n \geq 0} 1 \cdot \frac{x^n}{n!} = e^x$,</p>
<p>since there is exactly one set structure of any size, and</p>
<p>$\displaystyle E_2(x) = \frac{x^2}{2}$,</p>
<p>which is just the restriction of $E(x)$ to only the $x^2$ term. Of course, we also have $X(x) = x$. Putting this together, we calculate</p>
<p>$\begin{array}{rcl}\displaystyle (E \circ (E_2 + X))(x) &=& e^{(x^2/2 + x)} \\[1em] &=& \displaystyle \sum_{n \geq 0} \frac{(x^2/2 + x)^n}{n!} \\[1em] &=& \displaystyle \sum_{n \geq 0} \sum_{k = 0}^n \frac{1}{n!} \binom{n}{k} \left(\frac{x^2}{2}\right)^k x^{n-k} \\[1em] &=& \displaystyle \sum_{n \geq 0} \sum_{k=0}^n \frac{1}{n!} \binom{n}{k} \frac{x^{n+k}}{2^k} \end{array}$</p>
<p>Ultimately, we want something of the form $\displaystyle \sum_{m \geq 0} f_m \frac{x^m}{m!}$, so we’ll need to collect up like powers of $x$. To do that, we can do a bit of reindexing. Right now, the double sum is adding up a bunch of terms that can be thought of as making a triangle:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2015/06/56defd3db32555d9.png" alt="" /></p>
</div>
<p>Each ordered pair in the triangle corresponds to a single term being added. Each column corresponds to a particular value of $n$, with $n$ increasing to the right. Within each column, $k$ goes from $0$ up to $n$.</p>
<p>The powers of $x$ in our double sum are given by $n+k$. If we draw in lines showing terms that have the same power of $x$, it looks like this:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2015/06/8b6daafc45e4846d.png" alt="" /></p>
</div>
<p>So let’s choose a new variable $m$, defined by $m = n + k$. We can see that we will have terms for every $m \geq 0$. We will also keep the variable $k$ for our other index, and substitute $n = m - k$ to get rid of $n$. In other words, instead of adding up the triangle by columns, we are going to add it up by diagonals.</p>
<p>Previously we had $k \leq n$; substituting for $n$ that now turns into $k \leq m - k$. Adding $k$ to both sides and dividing by $2$ yields $k \leq \lfloor m/2 \rfloor$ (we can round down since $k$ is an integer). Looking at the diagram above, this makes sense: the height of each diagonal line is indeed half its index. Rewriting our indices of summation and substituting $m - k$ for $n$, we now have:</p>
<p>$\begin{array}{rcl}\displaystyle \sum_{n \geq 0} \sum_{k=0}^n \frac{1}{n!} \binom{n}{k} \frac{x^{n+k}}{2^k} &=& \displaystyle \sum_{m \geq 0} \sum_{k=0}^{\lfloor m/2 \rfloor} \frac{1}{(m-k)!} \binom{m-k}{k} \frac{x^m}{2^k} \\[1em] &=& \displaystyle \sum_{m \geq 0} \sum_{k=0}^{\lfloor m/2 \rfloor} \frac{1}{k!(m-2k)!} \frac{x^m}{2^k} \\[1em] &=& \displaystyle \sum_{m \geq 0} \frac{x^m}{m!} \sum_{k=0}^{\lfloor m/2 \rfloor} \frac{m!}{k!(m-2k)!2^k} \end{array}$</p>
<p>And hey, look at that! The coefficient of $x^m/m!$ is exactly what we previously came up with for $T_m$. Math works!</p>
<div class="references">

</div>

