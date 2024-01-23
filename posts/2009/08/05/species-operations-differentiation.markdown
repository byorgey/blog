---
title: 'Species operations: differentiation'
published: 2009-08-06T03:26:34Z
categories: combinatorics,haskell,math
tags: combinatorial species,differentiation
---

Continuing my series describing my new <a href="http://hackage.haskell.org/package/species">combinatorial species library</a>, today we'll take a look at the operation of <i>differentiation</i>.

You may remember that the <code>Species</code> type class has an <code>Algebra.Differential</code> constraint, which, <a href="http://byorgey.wordpress.com/2009/07/30/primitive-species-and-species-operations/">as I previously explained</a>, transitively implies an <code>Algebra.Ring</code> constraint.  But we haven't yet talked about the <code>Differential</code> contraint itself, which requires a method <code>differentiate :: Species s =&gt; s -&gt; s</code> (which I will abbreviate using the standard "prime" notation), which should satisfy

$(x * y)' \equiv x' * y + x * y'$

(up to isomorphism).  Okay, this is just the normal product rule for differentiation, from calculus---but what on earth could such a thing mean <i>combinatorially</i>?

There is actually a nice, simple answer: an $F'$-structure on the underlying set $U$ consists of an $F$-structure on $U \cup \{*\}$, where $*$ is a distinguished element distinct from all the elements of $U$.  To make the connection to <a href="http://en.wikibooks.org/wiki/Haskell/Zippers#Differentiation_of_data_types">data type differentiation</a>, we can also think of $*$ as a "hole".

[caption id="attachment_271" align="aligncenter" width="400" caption="Species differentiation"]<img src="http://byorgey.files.wordpress.com/2009/08/diff.png" alt="Species differentiation" title="diff" width="400" height="92" class="size-full wp-image-271" />[/caption]

The above diagram illustrates the situation: an $F'$-structure on $\{1,2,3,4,5\}$ is an $F$-structure on $\{1,2,3,4,5,*\}$.

And how about the law $(F * G)' \equiv F' * G + F * G'$?  Does this make combinatorial sense? (You may want to stop and think about it before reading on!)

By definition, an $(F * G)'$-structure on $U$ is an $(F*G)$-structure on $U \cup \{*\}$, which is a pair of an $F$-structure and a $G$-structure on a splitting (a two-partition) of $U \cup \{*\}$.  The distinguished $*$ label must end up on one side or the other, so an $(F*G)'$-structure can arise in one of two ways: it is either an $F'$-structure paired with a $G$-structure, or an $F$-structure paired with a $G'$-structure, depending on where the $*$ ends up.  But this is precisely saying that $(F * G)' \equiv F' * G + F * G'$!

Where does species differentiation show up?  The most well-known place is in defining the species $L$ of <i>lists</i> (linear orderings).  In fact,


$L = C'$,


that is, the species $L$ is the derivative of the <a href="http://byorgey.wordpress.com/2009/07/31/primitive-species-and-species-operations-part-ii/">species $C$ of cycles</a>.  A cycle defines an ordering, but there is no distinguished beginning or end; by making a cycle out of some elements with a distinguished extra element $*$, we uniquely identify a beginning/end of the ordering on the original elements: a list!

[caption id="attachment_274" align="aligncenter" width="400" caption="Differentiating a cycle to get a list"]<img src="http://byorgey.files.wordpress.com/2009/08/cyclediff.png" alt="Differentiating a cycle to get a list" title="cyclediff" width="400" height="137" class="size-full wp-image-274" />[/caption]

<code>
&gt; take 10 . labelled $ lists
[1,1,2,6,24,120,720,5040,40320,362880]
&gt; take 10 . labelled $ oneHole cycles
[1,1,2,6,24,120,720,5040,40320,362880]
&gt; generate lists ([1..3] :: [Int])
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
&gt; generate (oneHole cycles) ([1..3] :: [Int])
[&lt;*,1,2,3&gt;,&lt;*,1,3,2&gt;,&lt;*,2,1,3&gt;,&lt;*,2,3,1&gt;,&lt;*,3,1,2&gt;,&lt;*,3,2,1&gt;]
</code>

Here's an example of differentiation in action.  In the species library, the function <code>oneHole</code> is provided as a synonym for <code>differentiate</code>.  The session above shows that there are the same number of labelled lists as labelled one-hole cycles: this isn't surprising given the discussion above, and in fact, <code>list</code> is actually implemented as <code>oneHole cycle</code>.  Actually, this is a tiny lie, as the rest of the session shows: since lists are such a common combinatorial structure, there is a special case for them in the generation code.  But we can explicitly generate one-hole cycles as above; it's easy to see that they are in one-to-one correspondence with the lists.

To finish off this post, a few exercises for you (you can check your answers with the species library):
<ol>
	<li>Describe the species $1'$.</li>
	<li>Describe the species $X'$.</li>
	<li>Describe the species $E'$.</li>
	<li>Does differentiation distribute over addition?  That is, is it true that $(F + G)' \equiv F' + G'$ for any species $F$ and $G$?  Give a combinatorial interpretation of this identity, or say why it does not hold.</li>
	<li>Describe the species $L'$.</li>
	<li>Describe the species $C^{(n)}$ (i.e. the nth derivative of the species of cycles).</li>
</ol>




