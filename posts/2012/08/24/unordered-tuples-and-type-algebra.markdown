---
katex: true
title: 'Unordered tuples and type algebra'
published: 2012-08-24T18:41:46Z
categories: combinatorics
tags: algebra,discrete calculus,tuples,types,unordered
---

<p>At <a href="http://www.haskell.org/haskellwiki/Hac_%cf%86">Hac Phi</a> a few weekends ago (which, by the way, was awesome), Dan Doel told me about a certain curiosity in type algebra, and we ended up working out a bunch more details together with Gershom Bazerman, Scott Walck, and probably a couple others I'm forgetting. I decided to write up what we discovered. I have no idea what (if any) of this is really novel, but it was new to me at least.</p>
<h2 id="the-setup">The Setup</h2>
<p>I'll assume you're already familiar with the basic ideas of the <a href="http://blog.lab49.com/archives/3011">algebra of types</a>---$0$ represents the void type, $1$ represents the unit type, sum is tagged union, product is (ordered) pairing, and so on.</p>
<p>Given a type $T$, since product represents pairing, we can write $T^n$ to represent ordered $n$-tuples of $T$ values. Well, how about <em>unordered</em> $n$-tuples of $T$ values? Since there are $n!$ possible ways to order $n$ values, it seems that perhaps we could somehow divide by $n!$ to &quot;quotient out&quot; by the symmetries we want to disregard: $T^n/n!$.</p>
<p>If you've never seen this sort of thing before it is certainly not at all obvious that this makes any sense! But bear with me for a minute. At the very least, we can say that if this is to make sense we ought to be able to use these sorts of type expressions to calculate the <em>number of inhabitants</em> of a finite type. So, let's try it. For now let's take $T$ = <code>Bool</code> = $2$. I'll draw the elements of <code>Bool</code> as <img src="http://byorgey.files.wordpress.com/2012/08/f4fb3e7eaab3449bd6001af41a0f483e.png" /> and <img src="http://byorgey.files.wordpress.com/2012/08/bff2a4e3d70aa227274c29acabd24874.png" />.</p>
<p>There are clearly four different ordered pairs of <code>Bool</code>:</p>
<div style="text-align:center;">
<div class="figure">
<img src="http://byorgey.files.wordpress.com/2012/08/57082e8b3172c74efd77a3fb46773f72.png" /><p class="caption"></p>
</div>
</div>
<p>$T^n$ is supposed to represent ordered $n$-tuples of $T$, and indeed, $2^2 = 4$. How about unordered pairs? Since we don't care about the order I'll just choose to canonically sort all the $T$s to the front, followed by all the $F$s. It's not hard to see that there are three unordered pairs of <code>Bool</code>:</p>
<div style="text-align:center;">
<div class="figure">
<img src="http://byorgey.files.wordpress.com/2012/08/b581a5b1523f80906ce8ebd2358f7bca.png" /><p class="caption"></p>
</div>
</div>
<p>(To distinguish them from ordered tuples, I'll draw unordered tuples as above, with the elements slightly separated and a gray circle drawn round them.)</p>
<p>However, when we substitute $T = n = 2$ into $T^n/n!$, we get not $3$, but $(2^2)/2 = 2$. What gives?</p>
<h2 id="the-problem">The problem</h2>
<p>The problem is that $T^n/n!$ is only correct if all the values in the tuples are <em>distinct</em>. Then we overcount each unordered tuple by exactly a factor of $n!$---namely, all the $n!$ many permutations of the tuple, each of which is distinct as an ordered tuple. However, when some of the tuples have repeated elements, there can be fewer than $n!$ distinct ordered variants of a given unordered tuple. For example, the unordered tuple <img src="http://byorgey.files.wordpress.com/2012/08/4f56288955edc2f7e4e9bfe64c20bb19.png" /> has only $3$ (rather than $3! = 6$) ordered variants, namely</p>
<div style="text-align:center;">
<div class="figure">
<img src="http://byorgey.files.wordpress.com/2012/08/d422c36b3c2ea918ed1fe2bf5ce209b4.png" /><p class="caption"></p>
</div>
</div>
<p>because the two <img src="http://byorgey.files.wordpress.com/2012/08/f4fb3e7eaab3449bd6001af41a0f483e.png" />'s are identical.</p>
<p>(As a small aside, when working in the theory of <em>combinatorial species</em> one is concerned not with actual data structures but with data structure <em>shapes</em> full of distinct <em>labels</em>---and the fact that the labels are distinct means that $T^n/n!$ is (in some sense) actually the correct way to talk about unordered tuples within the theory of species. More on this in another post, perhaps.)</p>
<p>If $T^n/n!$ is not the correct expression for unordered tuples, what is? In fact, Dan started off this whole thing by telling me the answer to this question---but he didn't understand <em>why</em> it is the answer; we then proceeded to figure it out. For the purposes of pedagogy I'll reverse the process, working up from first principles to arrive at the answer.</p>
<h2 id="counting-unordered-tuples">Counting unordered tuples</h2>
<p>The first order of business is to count unordered tuples. Given a set $T$, how many unordered $n$-tuples are there with elements drawn from $T$ (where repetition is allowed)? Again, since the order doesn't matter, we can canonically sort the elements of $T$ with all copies of the first element first, then all copies of the second element, and so on. For example, here is an unordered $8$-tuple with elements drawn from $T = 4 = \{$ <img src="http://byorgey.files.wordpress.com/2012/08/8fefeb3ffb94a8ee0d194a608c9d4fbe.png" />, <img src="http://byorgey.files.wordpress.com/2012/08/1276478a9d4f8202b74c3e8017bdde32.png" />, <img src="http://byorgey.files.wordpress.com/2012/08/009204bc277b42931ac22fa76c06dbf3.png" />, <img src="http://byorgey.files.wordpress.com/2012/08/aaa5b27c0b2deb02de847a7fd8603cfd.png" /> $\}$:</p>
<div style="text-align:center;">
<div class="figure">
<img src="http://byorgey.files.wordpress.com/2012/08/e292857711858519ec3c9acef3010c8e.png" /><p class="caption"></p>
</div>
</div>
<p>Now imagine placing &quot;dividers&quot; to indicate the places where <img src="http://byorgey.files.wordpress.com/2012/08/8fefeb3ffb94a8ee0d194a608c9d4fbe.png" /> changes to <img src="http://byorgey.files.wordpress.com/2012/08/1276478a9d4f8202b74c3e8017bdde32.png" />, <img src="http://byorgey.files.wordpress.com/2012/08/1276478a9d4f8202b74c3e8017bdde32.png" /> changes to <img src="http://byorgey.files.wordpress.com/2012/08/009204bc277b42931ac22fa76c06dbf3.png" />, and so on:</p>
<div style="text-align:center;">
<div class="figure">
<img src="http://byorgey.files.wordpress.com/2012/08/8efe1867ef79987ed6f0d50038bd2835.png" /><p class="caption"></p>
</div>
</div>
<p>(Note how there are two dividers between the last <img src="http://byorgey.files.wordpress.com/2012/08/1276478a9d4f8202b74c3e8017bdde32.png" /> and the first <img src="http://byorgey.files.wordpress.com/2012/08/aaa5b27c0b2deb02de847a7fd8603cfd.png" />, indicating that there are no occurrences of <img src="http://byorgey.files.wordpress.com/2012/08/009204bc277b42931ac22fa76c06dbf3.png" />.) In fact, given that the elements are canonically sorted, it is unnecessary to specify their actual identities:</p>
<div style="text-align:center;">
<div class="figure">
<img src="http://byorgey.files.wordpress.com/2012/08/b66c734186ebd8ef2604d3fdee82ceba.png" /><p class="caption"></p>
</div>
</div>
<p>So, we can see that unordered $8$-tuples with elements from $T = 4$ correspond bijectively to such arrangements of eight dots and three dividers. In general, unordered $n$-tuples are in bijection with arrangements of $n$ dots and $|T|-1$ dividers, and there are as many such arrangements as ways to choose the positions of the $|T|-1$ dividers from among the $n+|T|-1$ total objects, that is,</p>
<p><div style="text-align:center;">
$\displaystyle \binom{n+|T|-1}{|T|-1}$
</div></p>
<p>(As an aside, this is the same as asking for the number of ways to place $n$ indistinguishable balls in $|T|$ distinguishable boxes---the balls in box $i$ indicate the multiplicity of element $i$ in the unordered $n$-tuple. This is #4 in Gian-Carlo Rota's &quot;twelvefold way&quot;, and is discussed on page 15 of Richard Stanley's <em>Enumerative Combinatorics</em>, Volume I. See also <a href="http://mathlesstraveled.com/2009/04/14/distributing-cookies-solutions/">this blog post I wrote</a> explaining this and related ideas).</p>
<h2 id="so-what">So what?</h2>
<p>And now for a little algebra:</p>
<p><div style="text-align:center;">
$\displaystyle \begin{array}{cl} & \displaystyle \binom{n+|T|-1}{|T|-1} \\ & \\ = & \displaystyle \frac{(n+|T|-1)!}{n!(|T|-1)!} \\ & \\ = & \displaystyle \frac{(n+|T|-1)(n+|T|-2) \cdots (|T|)}{n!} \\ & \\ = & \displaystyle \frac{|T|(|T|+1)(|T|+2) \cdots (|T| + n-1)}{n!}\end{array}$
</div></p>
<p>The expression on top of the fraction is known as a <em>rising factorial</em> and can be abbreviated $|T|^{\overline{n}}$. (In general, $x^{\overline{n}} = x(x+1)(x+2) \dots (x+n-1)$, so $1^{\overline{n}} = n!$.) In the end, we have discovered that the number of unordered $n$-tuples of $T$ is $|T|^{\overline{n}}/n!$, which looks surprisingly similar to the naïve but incorrect $T^n / n!$. In fact, the similarity is no coincidence, and there are good reasons for using a notation for rising factorial similar to the notation for normal powers, as we shall soon see.</p>
<p>And indeed, the correct type expression for unordered $n$-tuples of values from $T$ is $T^{\overline{n}} / n! = T(T+1)(T+2) \dots (T+(n-1))/n!$. This means that if we consider the set of ordered $n$-tuples where the first element is drawn from $T$, the second element from $T$ plus some extra distinguished element, the third from $T$ plus two extra elements, and so on, there will be exactly $n!$ of them for every unordered $n$-tuple with elements drawn from $T$. (In fact, we would even expect there to be some nice function from these &quot;extended $n$-tuples&quot; to unordered $n$-tuples such that the preimage of every unordered $n$-tuple is a set of size exactly $n!$---just because combinatorics usually works out like that. Finding such a correspondence is left as an exercise for the reader.)</p>
<h2 id="a-detour">A detour</h2>
<p>Before we get back to talking about $T^{\overline{n}}/n!$, a slight detour. Consider the variant type expression $T^{\underline{n}}/n!$, where $x^{\underline{n}} = x(x-1)(x-2) \dots (x-n+1)$ is a <em>falling factorial</em>. What (if anything) does it represent?</p>
<p>Subtraction of types is problematic in general (without resorting to <a href="http://byorgey.wordpress.com/2010/11/24/species-subtraction-made-simple/">virtual species</a>), but in this case we can interpret $T(T-1)(T-2) \dots$ as an ordered $n$-tuple with <em>no duplicate values</em>. We can choose any element of $T$ to go first, then any but the first element to go second, then any but the first two, and so on. This can in fact be made rigorous from the perspective of types, without involving virtual species---see <a href="http://blog.sigfpe.com/2007/09/type-of-distinct-pairs.html">Dan Piponi's blog post on the subject</a>.</p>
<div style="text-align:center;">
<div class="figure">
<img src="http://byorgey.files.wordpress.com/2012/08/401c69853b9a10e8f5ca59f2e9a49ba1.png" /><p class="caption"></p>
</div>
</div>
<h2 id="infinite-sums-and-discrete-calculus">Infinite sums and discrete calculus</h2>
<p>And now for some fun. If we sum $T^{\underline{n}}/n!$ over all $n$, it ought to represent the type of unordered tuples with distinct values of any length---that is, the type of sets over $T$.</p>
<div style="text-align:center;">
<div class="figure">
<img src="http://byorgey.files.wordpress.com/2012/08/37470019b9f01e754d0ccddc1cf9e203.png" /><p class="caption"></p>
</div>
</div>
<p><div style="text-align:center;">
$\displaystyle S(T) = 1 + T + \frac{T^{\underline{2}}}{2!} + \frac{T^{\underline{3}}}{3!} + \dots$
</div></p>
<p>Can we find a more compact representation for $S(T)$?</p>
<p>Consider the <em>forward difference</em> operator $\Delta$, defined by</p>
<p><div style="text-align:center;">
$\displaystyle \Delta f(x) = f(x+1) - f(x)$
</div></p>
<p>This is a discrete analogue of the familiar (continuous) differentiation operator from calculus. (For a good introduction to discrete calculus, see Graham <em>et al.</em>'s <a href="http://www.amazon.com/Concrete-Mathematics-Foundation-Computer-Science/dp/0201558025"><em>Concrete Mathematics</em></a>, one of my favorite math/CS books ever. See also the <a href="http://en.wikipedia.org/wiki/Finite_difference">Wikipedia page on finite differences</a>.) For our purposes we simply note that</p>
<p><div style="text-align:center;">
$\displaystyle \Delta x^{\underline{n}} = n x^{\underline{n-1}}$
</div></p>
<p>(proving this is not hard, and is left as an exercise). This is what justifies the notation for falling factorial: it is in some sense a discrete analogue of exponentiation!</p>
<p>The reason to bring $\Delta$ into the picture is that given the above identity for $\Delta$ applied to falling factorials, it is not hard to see that $S(T)$ is its own finite difference:</p>
<p><div style="text-align:center;">
$\displaystyle \Delta S(T) = S(T)$
</div></p>
<p>Expanding, we get $S(T+1) - S(T) = S(T)$ and hence $S(T+1) = 2 S(T)$. (Yes, I know, there's that pesky subtraction of types again; in the time-honored tradition of combinatorics we'll simply pretend it makes sense and trust there is a way to make it more formal!) Solving this recurrence together with the initial condition $S(0) = 1$ yields</p>
<p><div style="text-align:center;">
$\displaystyle S(T) = 2^T$
</div></p>
<p>which we can interpret as the space of functions from $T$ to <code>Bool</code>---that is, the type of sets over $T$, just like it should be! (Note that replacing falling factorial with exponentiation yields something which is its own derivative, with a solution of $e^T$---which indeed represents the <em>species</em> of sets, though it's harder to see what $e$ has to do with anything.)</p>
<p>Enough with the detour. What if we sum over $T^{\overline{n}}/n!$?</p>
<div style="text-align:center;">
<div class="figure">
<img src="http://byorgey.files.wordpress.com/2012/08/083c9ea314602745534823cdb89d3f58.png" /><p class="caption"></p>
</div>
</div>
<p><div style="text-align:center;">
$\displaystyle M(T) = 1 + T + \frac{T^{\overline{2}}}{2!} + \frac{T^{\overline{3}}}{3!} + \dots$
</div></p>
<p>There's a <em>backward difference</em> operator, $\nabla f(x) = f(x) - f(x-1)$, with the property that</p>
<p><div style="text-align:center;">
$\displaystyle \nabla x^{\overline{n}} = n x^{\overline{n-1}}$
</div></p>
<p>Hence $\nabla M(T) = M(T)$, <em>i.e.</em> $M(T) - M(T-1) = M(T)$, but here I am a bit stuck. Trying to solve this in a similar manner as before yields $M(T-1) = 0$, which seems bogus. $0$ is certainly not a solution, since $M(0) = 1$. I think in this case we are actually not justified in subtracting $M(T)$ from both sides, though I'd be hard-pressed to explain exactly why.</p>
<p>Intuitively, $M(T)$ ought to represent unordered tuples of $T$ of any length---that is, the type of <em>multisets</em> over $T$. This is isomorphic to the space of functions $T \to \mathbb{N}$, specifying the multiplicity of each element. I claim that $\mathbb{N}^T$ is in fact a solution to the above equation---though I don't really know how to <em>derive</em> it (or even what it really means).</p>
<p><div style="text-align:center;">
$\displaystyle \begin{array}{cl} & \displaystyle \mathbb{N}^T - \mathbb{N}^{T-1} \\ & \\ \cong & \displaystyle \mathbb{N}^{T-1}(\mathbb{N} - 1) \\ & \\ \cong & \displaystyle \mathbb{N}^{T-1} \mathbb{N} \\ & \\ \cong & \displaystyle \mathbb{N}^T \end{array}$
</div></p>
<p>The middle step notes that if you take one element away from the natural numbers, you are left with something which is still isomorphic to the natural numbers. I believe the above can all be made perfectly rigorous, but this blog post is already much too long as it is.</p>

