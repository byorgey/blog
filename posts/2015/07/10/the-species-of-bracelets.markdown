---
katex: true
title: 'The Species of Bracelets'
published: 2015-07-10T20:20:30Z
categories: combinatorics,haskell,math
tags: bracelets,generating functions,species
---

<p><em>Summary</em>: The <a href="http://hackage.haskell.org/package/species"><code>species</code> package</a> now has support for <em>bracelets</em>, <em>i.e.</em> equivalence classes of lists up to rotation and reversal. I show some examples of their use and then explain the (very interesting!) mathematics behind their implementation.</p>
<hr />
<p>I recently released a new version of my <a href="http://hackage.haskell.org/package/species"><code>species</code> package</a> which adds support for the species of <em>bracelets</em>. A bracelet is a (nonempty) sequence of items which is considered equivalent up to rotation and reversal. For example, the two structures illustrated below are considered equivalent as bracelets, since you can transform one into the other by a rotation and a flip:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2015/07/684db889ed37c01d.png" alt="" /></p>
</div>
<p>In other words, a bracelet has the same symmetry as a regular polygon—that is, its symmetry group is the <a href="https://en.wikipedia.org/wiki/Dihedral_group">dihedral group</a> $D_{2n}$. (Actually, this is only true for $n \geq 3$—I’ll say more about this later.)</p>
<p>Bracelets came up for me recently in relation to a fun side project (more on that soon), and I am told they also show up in applications in biology and chemistry (for example, bracelet symmetry shows up in molecules with cycles, which are common in organic chemistry). There was no way to derive the species of bracelets from what was already in the library, so I added them as a new primitive.</p>
<p>Let’s see some examples (later I discuss how they work). First, we set some options and imports.</p>
<pre><code><span style="color:gray;">ghci&gt; </span>:set -XNoImplicitPrelude
<span style="color:gray;">ghci&gt; </span>:m +NumericPrelude
<span style="color:gray;">ghci&gt; </span>:m +Math.Combinatorics.Species</code></pre>
<p>Unlabelled bracelets, by themselves, are completely uninteresting: there is only a single unlabelled bracelet shape of any positive size. (Unlabelled species <em>built using</em> bracelets can be interesting, however; we’ll see an example in just a bit). We can ask the library to tell us how many distinct size-$n$ unlabelled bracelets there are for $n \in \{0, \dots, 9\}$:</p>
<pre><code><span style="color:gray;">ghci&gt; </span>take 10 $ unlabelled bracelets
  [0,1,1,1,1,1,1,1,1,1]
</code></pre>
<p>Labelled bracelets are a bit more interesting. For $n \geq 3$ there are $(n-1)!/2$ labelled bracelets of size $n$: there are $(n-1)!$ cycles of size $n$ (there are $n!$ lists, which counts each cycle $n$ times, once for each rotation), and counting cycles exactly double counts bracelets, since each bracelet can be flipped in one of two ways. For example, there are $(5-1)!/2 = 24/2 = 12$ labelled bracelets of size $5$.</p>
<pre><code><span style="color:gray;">ghci&gt; </span>take 10 $ labelled bracelets
  [0,1,1,1,3,12,60,360,2520,20160]
</code></pre>
<p>In addition to counting these, we can exhaustively generate them (this is a bit annoying with the current API; I hope to improve it):</p>
<pre><code><span style="color:gray;">ghci&gt; </span>enumerate bracelets [0,1] :: [Bracelet Int]
  [&lt;&lt;0,1&gt;&gt;]

<span style="color:gray;">ghci&gt; </span>enumerate bracelets [0..2] :: [Bracelet Int]
  [&lt;&lt;0,1,2&gt;&gt;]

<span style="color:gray;">ghci&gt; </span>enumerate bracelets [0..3] :: [Bracelet Int]
  [&lt;&lt;0,1,2,3&gt;&gt;,&lt;&lt;0,1,3,2&gt;&gt;,&lt;&lt;0,2,1,3&gt;&gt;]
</code></pre>
<p>And here are all $12$ of the size-$5$ bracelets, where I’ve used a different color to represent each label (<a href="http://hub.darcs.net/byorgey/byorgey-wordpress/browse/2015-06-bracelets/bracelets.markdown">see here</a> for the code used to generate them):</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2015/07/9bf376e5e3d8e6ab.png" alt="" /></p>
</div>
<p>As a final example, consider the species $B \times E^2$, the Cartesian product of bracelets with ordered pairs of sets. That is, given a set of labels, we <em>simultaneously</em> give the labels a bracelet structure and <em>also</em> partition them into two (distinguishable) sets. Considering <em>unlabelled</em> structures of this species—that is, equivalence classes of labelled structures under relabelling—means that we can’t tell the labels apart, other than the fact that we can still tell which are in the first set and which are in the second. So, if we call the first set “purple” and the second “green”, we are counting the number of bracelets made from (otherwise indistinguishable) purple and green beads. Let’s call these <em>binary bracelets</em>. Here’s how many there are of sizes $0$ through $14$:</p>
<pre><code><span style="color:gray;">ghci&gt; </span>let biBracelets = bracelet &gt;&lt; (set * set)
<span style="color:gray;">ghci&gt; </span>take 15 $ unlabelled biBracelets
  [0,2,3,4,6,8,13,18,30,46,78,126,224,380,687]
</code></pre>
<p>Let’s use the <a href="https://oeis.org/">OEIS</a> to check that we’re on the right track:</p>
<pre><code><span style="color:gray;">ghci&gt; </span>:m +Math.OEIS
<span style="color:gray;">ghci&gt; </span>let res = lookupSequence (drop 1 . take 10 $ unlabelled biBracelets)
<span style="color:gray;">ghci&gt; </span>fmap description res
  Just "Number of necklaces with n beads of 2 colors, allowing turning over."
</code></pre>
<p>Unfortunately the <code>species</code> library can’t currently enumerate unlabelled structures of species involving Cartesian product, though I hope to fix that. But for now we can draw these purple-green bracelets with some <a href="http://hub.darcs.net/byorgey/byorgey-wordpress/browse/2015-06-bracelets/Bracelets.hs">custom enumeration code</a>. You can see the numbers $2, 3, 4, 6, 8, 13$ show up here, and it’s not too hard to convince yourself that each row contains all possible binary bracelets of a given size.</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2015/07/1fbe9a0e3687665b.png" alt="" /></p>
</div>
<p>If you’re just interested in <em>what you can do with</em> bracelets, you can stop reading now. If you’re interested in the mathematical and algorithmic details of how they are implemented, read on!</p>
<h2 id="exponential-generating-functions">Exponential generating functions</h2>
<p>The <em>exponential generating function</em> (egf) associated to a combinatorial species $F$ is defined by</p>
<p>$\displaystyle F(x) = \sum_{n \geq 0} |F[n]| \frac{x^n}{n!}.$</p>
<p>That is, the egf is an (infinite) formal power series where the coefficient of $x^n/n!$ is the number of distinct labelled $F$-structures on $n$ labels. We saw above that for $n \geq 3$ there are $(n-1)!/2$ labelled bracelets of size $n$, and there is one bracelet each of sizes $1$ and $2$. The egf for bracelets is thus:</p>
<p>$\displaystyle B(x) = x + \frac{x^2}{2} + \sum_{n \geq 3} \frac{(n-1)!}{2} \frac{x^n}{n!} = x + \frac{x^2}{2} + \sum_{n \geq 3} \frac{x^n}{2n}.$</p>
<p>(Challenge: show this is also equivalent to $\frac{1}{2}(x + x^2/2 - \ln(1-x))$.) This egf is <a href="https://github.com/byorgey/species/blob/master/Math/Combinatorics/Species/Labeled.hs#L51">directly encoded in the species library</a>, and this is what is being used to evaluate <code>labelled bracelets</code> in the example above.</p>
<p>Incidentally, the reason $(n-1)!/2$ only works for $n \geq 3$ is in some sense due to the fact that the <a href="https://en.wikipedia.org/wiki/Dihedral_group">dihedral groups</a> $D_2 = Z_2$ and $D_4 = K_4$ are a bit weird: every dihedral group $D_{2n}$ is a subgroup of the symmetric group $\mathcal{S}_n$ except for $D_2$ and $D_4$. The problem is that for $n < 3$, “flips” actually have no effect, as you can see below:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2015/07/10af4bdaed76ca0f.png" alt="" /></p>
</div>
<p>So, for example, $D_4$ has $4$ elements, corresponding to the identity, a 180 degree rotation, a flip, and a rotation + flip; but the symmetric group $\mathcal{S}_2$ only has two elements, in this case corresponding to the identity and a 180 degree rotation. The reason $(n-1)!/2$ doesn’t work, then, is that the division by two is superfluous: for $n < 3$, counting cycles doesn’t actually overcount bracelets, because every cycle is already a flipped version of itself. So it would also be correct (if rather baroque) to say that for $n < 3$ there are actually $(n-1)!$ bracelets.</p>
<p>I find this fascinating; it’s almost as if for bigger $n$ the dihedral symmetry has “enough room to breathe” whereas for small $n$ it doesn’t have enough space and gets crushed and folded in on itself, causing weird things to happen. It makes me wonder whether there are other sorts of symmetry with a transition from irregularity to regularity at even bigger $n$. Probably this is an easy question for a group theorist to answer but I’ve never thought about it before.</p>
<h2 id="ordinary-generating-functions">Ordinary generating functions</h2>
<p>The <em>ordinary generating function</em> (ogf) associated to a species $F$ is defined by</p>
<p>$\displaystyle \tilde F(x) = \sum_{n \geq 0} |F[n]/\mathord{\sim}| x^n$</p>
<p>where $\sim$ is the equivalence relation on $F$-structures induced by permuting the labels. That is, the coefficient of $x^n$ is the number of <em>equivalence classes</em> of $F$-structures on $n$ labels up to relabelling. There is only one unlabelled bracelet of any size $n \geq 1$, that is, any bracelet of size $n$ can be transformed into any other just by switching labels around. The unique unlabelled bracelet of a given size can be visualized as a bracelet of uniform beads:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2015/07/1dcdf822337298cd.png" alt="" /></p>
</div>
<p>though it’s occasionally important to keep in mind the more formal definition as an equivalence class of labelled bracelets. Since there’s just one unlabelled bracelet of each size, the ogf for bracelets is rather boring:</p>
<p>$\displaystyle \tilde B(x) = x + x^2 + x^3 + \dots = \frac{x}{x - 1}$.</p>
<p>This is <a href="https://github.com/byorgey/species/blob/master/Math/Combinatorics/Species/Unlabeled.hs#L45">encoded in the <code>species</code> library too</a>, and was used to compute <code>unlabelled bracelets</code> above.</p>
<h2 id="egfs-ogfs-and-homomorphisms">egfs, ogfs, and homomorphisms</h2>
<p>egfs are quite natural (in fact, species can be seen as a <a href="https://en.wikipedia.org/wiki/Categorification">categorification</a> of egfs), and the mapping from species to their associated egf is a homomorphism that preserves many operations such as sum, product, Cartesian product, composition, and derivative. ogfs, however, are not as nice. The mapping from species to ogfs preserves sum and product but does not, in general, preserve other operations like Cartesian product, composition or derivative. In some sense ogfs throw away too much information. Here’s a simple example to illustrate this: although the ogfs for bracelets and cycles are the same, namely, $x/(1-x)$ (there is only one unlabelled bracelet or cycle of each size), the ogfs for binary bracelets and binary cycles are different:</p>
<pre><code><span style="color:gray;">ghci&gt; </span>-- recall biBracelets = bracelet &gt;&lt; (set * set)
<span style="color:gray;">ghci&gt; </span>let biCycles = cycles &gt;&lt; (set * set)
<span style="color:gray;">ghci&gt; </span>take 15 $ unlabelled biBracelets
  [0,2,3,4,6,8,13,18,30,46,78,126,224,380,687]

<span style="color:gray;">ghci&gt; </span>take 15 $ unlabelled biCycles
  [0,2,3,4,6,8,14,20,36,60,108,188,352,632,1182]
</code></pre>
<p>(Puzzle: why are these the same up through $n=5$? Find the unique pair of distinct binary $6$-cycles which are equivalent as bracelets.)</p>
<p>Clearly, there is no way to take equal ogfs, apply the same operation to both, and get different results out. So the <code>species</code> library cannot be working directly with ogfs in the example above—something else must be going on. That something else is <em>cycle index series</em>, which generalize both egfs and ogfs, and retain enough information that they once again preserve many of the operations we care about.</p>
<h2 id="cycle-index-series">Cycle index series</h2>
<p>Let $\mathcal{S}_n$ denote the symmetric group of order $n$, that is, the group of permutations on $\{1, \dots, n\}$ under composition. It is well-known that every permutation $\sigma \in \mathcal{S}_n$ can be uniquely decomposed as a product of disjoint cycles. The <em>cycle type</em> of $\sigma$ is the sequence of natural numbers $\sigma_1, \sigma_2, \sigma_3, \dots$ where $\sigma_i$ is the number of $i$-cycles in the cycle decomposition of $\sigma$. For example, the permutation $(132)(45)(78)(6)$ has cycle type $1,2,1,0,0,0,\dots$ since it has one $1$-cycle, two $2$-cycles, and one $3$-cycle.</p>
<p>For a species $F$ and a permutation $\sigma \in \mathcal{S}_n$, let $\mathrm{fix}\ F[\sigma]$ denote the number of $F$-structures that are fixed by the action of $\sigma$, that is,</p>
<p>$\displaystyle \mathrm{fix}\ F[\sigma] = \#\{ f \in F[n] \mid F[\sigma] f = f \}.$</p>
<p>The <em>cycle index series</em> of a combinatorial species $F$ is a formal power series in an infinite set of variables $x_1, x_2, x_3, \dots$ defined by</p>
<p>$\displaystyle Z_F(x_1, x_2, x_3, \dots) = \sum_{n \geq 0} \frac{1}{n!} \sum_{\sigma \in \mathcal{S}_n} \mathrm{fix}\ F[\sigma] x_1^{\sigma_1} x_2^{\sigma_2} x_3^{\sigma_3} \dots$</p>
<p>We also sometimes write $x^\sigma$ as an abbreviation for $x_1^{\sigma_1} x_2^{\sigma_2} x_3^{\sigma_3} \dots$. As a simple example, consider the species of lists, <em>i.e.</em> linear orderings. For each $n$, the identity permutation (with cycle type $n,0,0,\dots$) fixes all $n!$ lists of length $n$, whereas all other permutations do not fix any lists. Therefore</p>
<p>$\displaystyle Z_L(x_1, x_2, x_3, \dots) = \sum_{n \geq 0} \frac{1}{n!} n! x_1^n = \sum_{n \geq 0} x_1^n = \frac{1}{1 - x_1}.$</p>
<p>(This is not really that great of an example, though—since lists are regular species, that is, they have no nontrivial symmetry, their cycle index series, egf, and ogf are all essentially the same.)</p>
<p>Cycle index series are linked to both egfs and ogfs by the identities</p>
<p>$\displaystyle \begin{array}{rcl}F(x) &=& Z_F(x,0,0,\dots) \\ \tilde F(x) &=& Z_F(x,x^2,x^3, \dots)\end{array}$</p>
<p>To show the first, note that setting all $x_i$ to $0$ other than $x_1$ means that the only terms that survive are terms with only $x_1$ raised to some power. These correspond to permutations with only $1$-cycles, that is, identity permutations. Identity permutations fix <em>all</em> $F$-structures of a given size, so we have</p>
<p>$\begin{array}{rcl}  Z_F(x,0,0,\dots) &=& \displaystyle \sum_{n \geq 0} \frac{1}{n!} \mathrm{fix}\ F[\mathit{id}] x^n \\  &=& \displaystyle \sum_{n \geq 0} |F[n]| \frac{x^n}{n!}. \end{array}$</p>
<p>To prove the link to ogfs, note first that for any permutation $\sigma \in \mathcal{S}_n$ with cycle type $\sigma_1,\sigma_2,\sigma_3,\dots$ we have $\sigma_1 + 2\sigma_2 + 3\sigma_3 + \dots = n$. Thus:</p>
<p>$\begin{array}{rcl}  Z_F(x,x^2,x^3,\dots) &=& \displaystyle \sum_{n \geq 0} \frac{1}{n!} \sum_{\sigma  \in \mathcal{S}_n} \mathrm{fix}\ F[\sigma]  x^{\sigma_1}x^{2\sigma_2}x^{3\sigma_3}\dots \\  &=& \displaystyle \sum_{n \geq 0} \frac{1}{n!} \sum_{\sigma \in \mathcal{S}_n} \mathrm{fix}\ F[\sigma] x^n \\  &=& \displaystyle \sum_{n \geq 0} |F[n]/\mathord{\sim}| x^n \end{array}$</p>
<p>where the final step is an application of <a href="https://en.wikipedia.org/?title=Burnside%27s_lemma">Burnside’s Lemma</a>.</p>
<p>The important point is that the mapping from species to cycle index series is again a homomorphism for many of the operations we care about, including Cartesian product and composition. So in order to compute an ogf for some species defined in terms of operations that are not compatible with ogfs, one can start out computing with cycle index series and then project down to an ogf at the end.</p>
<h2 id="cycle-index-series-for-bracelets">Cycle index series for bracelets</h2>
<p>Let’s now see how to work out the cycle index series for the species of bracelets. For $n=1$, the single bracelet is fixed by the only element of $\mathcal{S}_1$, giving a term of $x_1$. For $n=2$, the single bracelet is fixed by both elements of $\mathcal{S}_2$, one of which has cycle type $2,0,0,\dots$ and the other $0,1,0,\dots$. Bracelets of size $n \geq 3$, as discussed previously, have the dihedral group $D_{2n}$ as their symmetry group. That is, every one of the $(n-1)!/2$ size-$n$ bracelets is fixed by the action of each element of $D_{2n}$, and no bracelets are fixed by the action of any other permutation. Putting this all together, we obtain</p>
<p>$\begin{array}{rcl} Z_B(x_1, x_2, x_3, \dots) &=& \displaystyle \sum_{n \geq 0}  \frac{1}{n!} \sum_{\sigma \in \mathcal{S}_n} \mathrm{fix}\ B[\sigma]  x_1^{\sigma_1}x_2^{\sigma_2}x_3^{\sigma_3}\dots \\ &=& \displaystyle x_1 + \frac{1}{2}(x_1^2 + x_2) + \sum_{n \geq 3}  \frac{1}{n!} \sum_{\sigma \in D_{2n}} \frac{(n-1)!}{2} x^\sigma \\ &=& \displaystyle x_1 + \frac{1}{2}(x_1^2 + x_2) + \sum_{n \geq 3}  \frac{1}{2n} \sum_{\sigma \in D_{2n}} x^\sigma. \end{array}$</p>
<p>Our remaining task is thus to compute $\sum_{\sigma \in D_{2n}} x^\sigma$, that is, to compute the cycle types of elements of $D_{2n}$ for $n \geq 3$. I don’t know whether there’s a nice closed form for $Z_B$, but for our purposes it doesn’t matter: it suffices to come up with a finite algorithm to generate all its terms with their coefficients. A closed form might be important if we want to compute with $Z_B$ symbolically, but if we just want to generate coefficients, an algorithm is good enough.</p>
<p>In general, $D_{2n}$ has $n$ elements corresponding to rotations (including the identity element, which we think of as a rotation by $0$ degrees) and $n$ elements corresponding to reflections across some axis. Below I’ve drawn illustrations showing the symmetries of bracelets of size $5$ and $6$; each symmetry corresponds to an element of $D_{2n}$.</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2015/07/8977083873494c44.png" alt="" /></p>
</div>
<p>The lines indicate reflections. You can see that in general there are $n$ lines of reflection. The curved arrows indicate clockwise rotations; taking any number of consecutive arrows from $0$ to $n-1$ gives a distinct rotational symmetry. Let’s label the rotations $R_k$ (for $k \in \{0, \dots, n-1\}$), where $R_k$ indicates a rotation by $k/n$ of a turn (so $R_0$ is the identity element). We won’t bother labelling the reflections since it’s not clear how we would choose canonical names for them, and in any case (as we’ll see) we don’t have as much of a need to give them names as we do for the rotations. The only thing we will note is that for even $n$ there are two distinct types of reflections, as illustrated by the dark and light blue lines on the right: the dark blue lines pass through two vertices, and the light blue ones pass through two edges. In the odd case, on the other hand, every line of reflection passes through one vertex and one edge. If you haven’t studied dihedral groups before, you might want to take a minute to convince yourself that this covers all the possible symmetries. It’s clear that a rotation followed by a rotation is again a rotation; what may be less intuitively clear is that a reflection followed by a reflection is a rotation, and that a rotation followed by a reflection is a reflection.</p>
<p>So the name of the game is to consider each group element as a permutation of the labels, and compute the cycle type of the permutation. Let’s tackle the reflections first; we have to separately consider the cases when $n$ is odd and even. We saw above that when $n = 2m+1$ is odd, each line of reflection passes through exactly one vertex. As a permutation, that means the reflection will fix the label at the vertex it passes through, and swap the labels on other vertices in pairs, as shown in the leftmost diagram below:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2015/07/24f22b9afa9d6f81.png" alt="" /></p>
</div>
<p>So the permutation has cycle type $1,m,0,0,\dots$. There is one $1$-cycle, and the remaining $n-1 = 2m$ elements are paired off in $2$-cycles. There are $n$ of these reflections in total, yielding a term of $nx_1x_2^m$ (where $m = \lfloor n/2 \rfloor$).</p>
<p>When $n = 2m$ is even, half of the reflections (the light blue ones) have no fixed points, as in the middle diagram above; they put everything in $2$-cycles. The other half of the even reflections fix two vertices, with the rest in $2$-cycles, as in the rightmost diagram above. In all, this yields terms $mx_1^2x_2^{m-1} + mx_2^m$.</p>
<p>Now let’s tackle the rotations. One could be forgiven for initially assuming that each rotation will just yield one big $n$-cycle… a rotation is just cycling the vertices, right? But it is a bit more subtle than that. Let’s look at some examples. In each example below, the green curved arrow indicates a rotation $R_k$ applied to the bracelet. As you can check, the other arrows show the resulting permutation on the labels, that is, each arrow points from one node to the node where it ends up under the action of the rotation.</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2015/07/c325b280c626b005.png" alt="" /></p>
</div>
<p>Do you see the pattern? In the case when $k=1$ (the first example above), or more generally when $n$ and $k$ are relatively prime (the second example above, with $n=5$ and $k=2$), $R_k$ indeed generates a single $n$-cycle. But when $n$ and $k$ are not relatively prime, it generates multiple cycles. By symmetry the cycles must all be the same size; in general, the rotation $R_k$ generates $(n,k)$ cycles of size $n/(n,k)$ (where $(n,k)$ denotes the greatest common divisor of $n$ and $k$). So, for example, $2$ cycles are generated when $n=6$ and $k=2$ or $k=4$ (the next two examples above). The last example shows $n=12$ and $k=3$; we can see that three $4$-cycles are generated. Note this even works when $k=0$: we have $(n,0) = n$, so we get $n$ cycles of size $n/n = 1$, <em>i.e.</em> the identity permutation.</p>
<p>So $R_k$ contributes a term $x_{n/(n,k)}^{(n,k)}$. However, we can say something a bit more concise than this. Note, for example, when $n=12$, as the contribution of all the $R_k$ we get</p>
<p>$x_1^{12} + x_{12} + x_6^2 + x_4^3 + x_3^4 + x_{12} + x_2^6 + x_{12} + x_3^4 + x_4^3 + x_6^2 + x_{12}$</p>
<p>but we can collect like terms to get</p>
<p>$x_1^{12} + x_2^6 + 2x_3^4 + 2x_4^3 + 2x_6^2 + 4x_{12}$</p>
<p>For a given divisor $d \mid n$, the coefficient of $x_{n/d}^d$ is the number of nonnegative integers less than $n$ whose $\gcd$ with $n$ is equal to $d$. For example, the coefficient of $x_{6}^2$ is $2$, since there are two values of $k$ for which $(n,k) = 12/6 = 2$ and hence generate a six-cycle, namely, $2$ and $10$. So as the contribution of the $R_k$ we could write something like</p>
<p>$\displaystyle \sum_{d \mid n} \left( \#\{ 1 \leq k \leq n \mid (k,n) = d \} \right) x_{n/d}^d$</p>
<p>but there is a better way. Note that</p>
<p>$\displaystyle \#\{ 1 \leq k \leq n \mid (k,n) = d \} = \#\{ 1 \leq j \leq n/d \mid (j,n/d) = 1 \}$</p>
<p>since multiplying and dividing by $d$ establishes a bijection between the two sets. For example, we saw that $2$ and $10$ are the two numbers whose $\gcd$ with $12$ is $2$; this corresponds to the fact that $2/2 = 1$ and $10/2 = 5$ are relatively prime to $12/2 = 6$.</p>
<p>But counting relatively prime numbers is precisely what Euler’s totient function (usually written $\varphi$) does. So we can rewrite the coefficient of $x_{n/d}^d$ as</p>
<p>$\displaystyle \varphi(n/d) x_{n/d}^d$.</p>
<p>Finally, since we are adding up these terms for all divisors $d \mid n$, we can swap $d$ and $n/d$ (divisors of $n$ always come in pairs whose product is $n$), and rewrite this as</p>
<p>$\displaystyle \varphi(d) x_d^{n/d}$.</p>
<p>To sum up, then, we have for each $n \geq 3$:</p>
<ol style="list-style-type:decimal;">
<li>When $n$ is odd, $n x_1 x_2^{\lfloor n/2 \rfloor}$.</li>
<li>When $n$ is even, $(n/2) x_1^2 x_2^{n/2 - 1} + (n/2)x_2^{n/2}$.</li>
<li>For each $d \mid n$, we have $\varphi(d) x_d^{n/d}$.</li>
</ol>
<p>The only overlap is between (2) and (3): when $d = 2$ both generate $x_2^{n/2}$ terms. Using <a href="https://en.wikipedia.org/wiki/Iverson_bracket">Iverson brackets</a> (the notation $[P]$ is equal to $1$ if the predicate $P$ is true, and $0$ if it is false), we can thus write the sum of the above for a particular $n$ as</p>
<p>$\displaystyle [n\text{ odd}](n x_1 x_2^{\lfloor n/2 \rfloor}) + [n\text{ even}]\left(\frac n 2 x_1^2 x_2^{n/2 - 1}\right) + \sum_{d \mid n} \left(\varphi(d) + [d = 2]\frac n 2\right) x_d^{n/d}$.</p>
<p>Substituting this for $\displaystyle \sum_{\sigma \in D_{2n}} x^\sigma$ yields a full definition of $Z_B$. You can see the result <a href="https://github.com/byorgey/species/blob/master/Math/Combinatorics/Species/CycleIndex.hs#L136-L151">encoded in the species library here</a>. Here’s the beginning of the full expanded series:</p>
<pre><code><span style="color:gray;">ghci&gt; </span>:m +Math.Combinatorics.Species.Types
<span style="color:gray;">ghci&gt; </span>take 107 $ show (bracelets :: CycleIndex)
  "CI x1 + 1 % 2 x2 + 1 % 2 x1^2 + 1 % 3 x3 + 1 % 2 x1 x2 + 1 % 6 x1^3 + 1 % 4 x4 + 3 % 8 x2^2 + 1 % 4 x1^2 x2"
</code></pre>
<p>This, then, is how <code>unlabelled biBracelets</code> (for example) is calculated, where <code>biBracelets = bracelet &gt;&lt; (set * set)</code>. The cycle index series for <code>bracelet</code> and <code>set</code> are combined according to the operations on cycle index series corresponding to <code>*</code> and <code>&gt;&lt;</code>, and then the resulting cycle index series is mapped down to an ogf by substituting $x^k$ for each $x_k$.</p>
<h2 id="bracelet-generation">Bracelet generation</h2>
<p>The final thing to mention is how bracelet <em>generation</em> works. Of course we can’t really generate actual bracelets, but only lists. Since bracelets can be thought of as equivalence classes of lists (under rotation and reversal), the idea is to pick a canonical representative element of each equivalence class, and generate those. A natural candidate is the <em>lexicographically smallest</em> among all rotations and reversals (assuming the labels have an ordering; if they don’t we can pick an ordering arbitrarily). One easy solution would be to generate all possible lists and throw out the redundant ones, but that would be rather inefficient. It is surprisingly tricky to do this efficiently. Fortunately, there is a series of papers by Joe Sawada (<a href="http://www.cis.uoguelph.ca/~sawada/papers/fix-brace.pdf">Generating bracelets with fixed content</a>; <a href="http://www.cis.uoguelph.ca/~sawada/papers/alph.pdf">A fast algorithm to generate necklaces with fixed content</a>; <a href="http://www.cis.uoguelph.ca/~sawada/papers/brace.pdf">Generating bracelets in constant amortized time</a>) describing (and proving correct) some efficient algorithms for generating things like cycles and bracelets. In fact, they are as efficient as possible, theoretically speaking: they do only $O(1)$ work per cycle or bracelet generated. One problem is that the algorithms are very imperative, so they cannot be directly transcribed into Haskell. But I played around with benchmarking various formulations in Haskell and got it as fast as I could. (Interestingly, using <code>STUArray</code> was a lot slower in practice than a simple functional implementation, even though the imperative solution is asymptotically faster in theory—my functional implementation is at least $O(\lg n)$ per bracelet, and quite possibly $O(n)$, though since $n$ is typically quite small it doesn’t really matter very much. Of course it’s also quite possible that there are tricks to make the array version go faster that I don’t know about.) The result is released in the <a href="http://hackage.haskell.org/package/multiset%2Dcomb">multiset-comb package</a>; you can see the <a href="http://hub.darcs.net/byorgey/multiset-comb/browse/Math/Combinatorics/Multiset.hs#449">bracelet generation code here</a>.</p>
<div class="references">

</div>

