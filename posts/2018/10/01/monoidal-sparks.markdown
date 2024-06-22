---
katex: true
title: 'Monoidal sparks'
published: 2018-10-01T16:15:04Z
categories: math
tags: monoid,pairs,product,quadratic,spark
---

<p>While at <a href="https://icfp18.sigplan.org/">ICFP in St. Louis</a> this past week, I discovered an interesting construction on monoids that no one I talked to (including <a href="http://very.science/">Kenny Foner</a> and <a href="https://github.com/ekmett">Edward Kmett</a>) seemed to have heard of or thought about before. In this post I’ll present the abstract construction itself, and in another post I’ll explain the particular context in which I first came up with it. (Normally I would <a href="https://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/">put the examples first</a>, before explaining the idea in general; but I only know of <em>one</em> example so far, and I’m curious to see if anyone will come up with more. I don’t want to bias people’s thinking too much with my one example!)</p>
<p>The bigger context here is thinking about different ways of putting a monoid structure on a product type $A \times B$, assuming $A$ and $B$ are themselves monoids. Previously I knew of two ways to do this. The obvious way is to just have the two sides operate in parallel, without interacting at all: $(a_1,b_1) \diamond (a_2,b_2) = (a_1 \diamond a_2, b_1 \diamond b_2)$ and so on. Alternatively, if $A$ acts on $B$, we can form the <a href="http://ozark.hendrix.edu/~yorgey/pub/twisted.pdf">semidirect product</a>.</p>
<p>But here is a third way. Suppose $(A, \varepsilon_A, \diamond)$ is a monoid, and $(B, \varepsilon_B, \oplus)$ is a <em>commutative</em> monoid. To connect them, we also suppose there is another binary operation $- \cdot - : A \to A \to B$, which I will pronounce “spark”. The way I like to think of it is that two values of type $A$, in addition to combining to produce another $A$ via the monoid operation, also produce a “spark” of type $B$. That is, values of type $B$ somehow capture information about the interaction between pairs of $A$ values.</p>
<p>Sparking needs to interact sensibly with the monoid structures on $A$ and $B$: in particular, fixing either argument must result in a monoid homomorphism from $A$ to $B$. That is, for any choice of $a \in A$, we have $a \cdot \varepsilon_A = \varepsilon_B$ (<em>i.e.</em> $\varepsilon_A$ is boring and can never produce a nontrivial spark with anything else), and $a \cdot (a_1 \diamond a_2) = (a \cdot a_1) \oplus (a \cdot a_2)$ (<em>i.e.</em> sparking commutes with the monoid operations). Similar laws should hold if we fix the second argument of $- \cdot -$ instead of the first.</p>
<p>Given all of this, we can now define a monoid on $A \times B$ as follows:</p>
<p>$(a_1, b_1) \otimes (a_2, b_2) = (a_1 \diamond a_2, b_1 \oplus b_2 \oplus (a_1 \cdot a_2))$</p>
<p>That is, we combine the $A$ values normally, and then we combine the $B$ values <em>together with</em> the “spark” of type $B$ produced by the two $A$ values.</p>
<p>Let’s see that this does indeed define a valid monoid:</p>
<ul>
<li><p>The identity is $(\varepsilon_A, \varepsilon_B)$, since</p>
<p>$(\varepsilon_A, \varepsilon_B) \otimes (a,b) = (\varepsilon_A  \diamond a, \varepsilon_B \oplus b \oplus (\varepsilon_A \cdot a)) =  (a, b \oplus \varepsilon_B) = (a,b)$</p>
<p>Notice how we used the identity laws for $A$ (once) and $B$ (twice), as well as the law that says $\varepsilon_A \cdot a =  \varepsilon_B$. The proof that $(\varepsilon_A, \varepsilon_B)$ is a right identity for $\otimes$ is similar.</p></li>
<li><p>To see that the combining operation is associative, we can reason as follows:</p>
<p>$\begin{array}{rcl} & & ((a_1,b_1) \otimes (a_2,b_2)) \otimes (a_3,b_3) \\[0.5em] & = & \qquad \text{\{expand definition of \begin{math}\otimes\end{math}\}} \\[0.5em] & & (a_1 \diamond a_2, b_1 \oplus b_2 \oplus (a_1 \cdot a_2)) \otimes (a_3,b_3) \\[0.5em] & = & \qquad \text{\{expand definition of \begin{math}\otimes\end{math} again\}} \\[0.5em] & & (a_1 \diamond a_2 \diamond a_3, b_1 \oplus b_2 \oplus (a_1 \cdot a_2) \oplus b_3 \oplus ((a_1 \diamond a_2) \cdot a_3)) \\[0.5em] & = & \qquad \text{\{\begin{math}- \cdot a_3\end{math} is a homomorphism, \begin{math}\oplus\end{math} is commutative\}} \\[0.5em] & & (a_1 \diamond a_2 \diamond a_3, b_1 \oplus b_2 \oplus b_3 \oplus (a_1 \cdot a_2) \oplus (a_1 \cdot a_3) \oplus (a_2 \cdot a_3)) \end{array}$</p>
<p>and</p>
<p>$\begin{array}{rcl} & & (a_1,b_1) \otimes ((a_2,b_2) \otimes (a_3,b_3)) \\[0.5em] & = & \qquad \text{\{expand definition of \begin{math}\otimes\end{math}\}} \\[0.5em] & & (a_1,b_1) \otimes (a_2 \diamond a_3, b_2 \oplus b_3 \oplus (a_2 \cdot a_3)) \\[0.5em] & = & \qquad \text{\{expand definition of \begin{math}\otimes\end{math} again\}} \\[0.5em] & & (a_1 \diamond a_2 \diamond a_3, b_1 \oplus (b_2 \oplus b_3 \oplus (a_2 \cdot a_3)) \oplus (a_1 \cdot (a_2 \diamond a_3))) \\[0.5em] & = & \qquad \text{\{\begin{math}a_1 \cdot -\end{math} is a homomorphism, \begin{math}\oplus\end{math} is commutative\}} \\[0.5em] & & (a_1 \diamond a_2 \diamond a_3, b_1 \oplus b_2 \oplus b_3 \oplus (a_1 \cdot a_2) \oplus (a_1 \cdot a_3) \oplus (a_2 \cdot a_3)) \end{array}$</p>
<p>In a formal proof one would have to also explicitly note uses of associativity of $\diamond$ and $\oplus$ but I didn’t want to be that pedantic here.</p></li>
</ul>
<p>In addition, if $A$ is a commutative monoid and the spark operation $- \cdot -$ commutes, then the resulting monoid $(A \times B, \otimes)$ will be commutative as well.</p>
<p>The proof of associativity gives us a bit of insight into what is going on here. Notice that when reducing $(a_1,b_1) \otimes (a_2,b_2) \otimes (a_3,b_3)$, we end up with all possible sparks between pairs of $a$’s, <em>i.e.</em> $(a_1 \cdot a_2) \oplus (a_1 \cdot a_3) \oplus (a_2 \cdot a_3)$, and one can prove that this holds more generally. In particular, if we start with a list of $A$ values:</p>
<p>$[a_1, a_2, \dots, a_n]$,</p>
<p>then inject them all into $A \times B$ by pairing them with $\varepsilon_B$:</p>
<p>$[(a_1, \varepsilon_B), (a_2, \varepsilon_B), \dots, (a_n, \varepsilon_B)]$,</p>
<p>and finally fold this list with $\otimes$, the second element of the resulting pair is</p>
<p>$\displaystyle \bigoplus_{i \neq j} (a_i \cdot a_j)$,</p>
<p>that is, the combination (via the monoid on $B$) of the sparks between <em>all possible pairings</em> of the $a_i$. Of course there are $O(n^2)$ such pairings: the point is that whereas computing this via a straightforward fold over the list may well take $O(n^2)$ time, by using a balanced fold (<em>i.e.</em> splitting the list in half recursively and then combining from the leaves up) it may be possible to compute it in $O(n \log n)$ time instead (at least, this is the case for the example I have in mind!).</p>
<p>Please leave a comment if you can you think of any instances of this pattern, or if you have seen this pattern discussed elsewhere. In a future post I will write about the instance I had in mind when coming up with this generalized formulation.</p>

