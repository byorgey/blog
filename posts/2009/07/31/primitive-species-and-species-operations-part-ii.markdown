---
title: Primitive species and species operations, part II
published: 2009-08-01T01:31:03Z
categories: combinatorics,haskell,math
tags: combinatorial species,cycles,sets,singletons
---

In <a href="http://byorgey.wordpress.com/2009/07/30/primitive-species-and-species-operations/">my previous post</a>, I began describing the primitive species and species operations supported by my <a href="http://hackage.haskell.org/package/species">combinatorial species library</a>; we looked at the ring structure on species, that is, the primitive species $latex 0$ and $latex 1$, and the operations of species sum and product.  Today we'll continue by looking at a few more primitive species: <i>singletons</i>, <i>sets</i>, and <i>cycles</i>.

[By the way, all the diagrams for this post and the previous one were generated programmatically using my <a href="http://code.haskell.org/diagrams/">diagrams</a> library.  I'll probably put the code up as an example on the diagrams website sometime in the near future.]

<h3>X</h3>

The species $latex X$, also known as the species of <i>singletons</i>, is picky in a way similar to the species $latex 1$.  Whereas $latex 1$ only puts a structure on the empty set of labels, X only puts a (single) structure on singleton label sets.  If you give it more than one label, or none, it turns up its nose and refuses to do anything.

[caption id="attachment_227" align="aligncenter" width="400" caption="The species X of singletons"]<img src="http://byorgey.files.wordpress.com/2009/07/singleton.png" alt="The species X of singletons" title="singleton" width="400" height="92" class="size-full wp-image-227" />[/caption]

<code>
&gt; take 10 $ labelled singleton
[0,1,0,0,0,0,0,0,0,0]
&gt; generate singleton (['a'] :: [Char])
['a']
&gt; generate singleton ("abc" :: [Char])
[]
</code>

A few exercises: try to work them out yourself, then use the species library to check if you are correct!
<ol>
	<li>Describe the species $latex X + X$.  Show that it is isomorphic to the species $latex 2 * X$.</li>
	<li>Describe the species $latex X * X$.</li>
</ol>

<h3>E</h3>

The species $latex E$ of sets, on the other hand, isn't picky at all: it will happily put a singleton structure on any label set.  Usually we identify this structure with the label set itself; that is, the only $latex E$-structure on a label set $latex U$ is $latex U$ itself.

[caption id="attachment_258" align="aligncenter" width="400" caption="The species E of sets"]<img src="http://byorgey.files.wordpress.com/2009/07/set1.png" alt="The species E of sets" title="set" width="400" height="92" class="size-full wp-image-258" />[/caption]

<code>
&gt; take 10 $ labelled sets
[1,1,1,1,1,1,1,1,1,1]
&gt; take 10 $ unlabelled sets
[1,1,1,1,1,1,1,1,1,1]
&gt; generate set ([1..3] :: [Int])
[{1,2,3}]
&gt; generate set ([] :: [Int])
[{}]
</code>

We can now also describe the derived species $latex X * E$ of <i>elements</i>, also known as the species of <i>pointed sets</i>.  The only way to get any $latex X * E$ structures is by partitioning the label set $latex U$ into a singleton and all the rest, in which case we get exactly one structure; so there is one $latex X * E$ structure for each element of $latex U$.

<code>
&gt; take 10 $ labelled (x * set)
[0,1,2,3,4,5,6,7,8,9]
&gt; take 10 $ unlabelled (x * set)
[0,1,1,1,1,1,1,1,1,1]
&gt; generate (x * set) ([1..3] :: [Int])
[(1,{2,3}),(2,{1,3}),(3,{1,2})]
</code>

(<code>x</code> is just a synonym for <code>singleton</code>.)  Noteworthy is the fact that this is the first species we've looked at which has different numbers of labelled and unlabelled structures!  This makes sense: there are $latex n$ labelled $latex (X * E)$-structures on a size $latex n$ set; but if we can't tell the difference between the labels, any one of them is just as good as any other, so we only get one unlabelled structure (unless the label set is empty, when we don't get any structures: the $latex X$ still requires us to have at least one element!).  Note also that <code>element</code> is a special synonym for <code>x * set</code> with a special semantics under <code>generate</code>: if we really want to pick <i>elements</i> of the label set, then we probably don't want to actually see each element paired with a set of the leftover elements, we just want to see the element itself:

<code>
&gt; generate elements ([1..3]::[Int])
[1,2,3]
</code>

<h3>C</h3>

The final primitive species---and the only one so far that doesn't feel quite so utterly trivial---is the species $latex C$ of <i>cycles</i>.  $latex C$ puts no structures on an empty label set, but given any non-empty label set, $latex C$ generates the set of all <i>cyclical orderings</i> of the labels.  

[caption id="attachment_259" align="aligncenter" width="400" caption="The species C of cycles"]<img src="http://byorgey.files.wordpress.com/2009/07/cycle1.png" alt="The species C of cycles" title="cycle" width="400" height="92" class="size-full wp-image-259" />[/caption]

Of course, the above diagram only shows six of the cycle structures on five labels; the ellipsis is meant to suggest the others not shown. So... how many labelled cycle structures <i>are</i> there on five labels, or generally on $latex n$ labels?  (Of course there is only one <i>un</i>labelled $latex n$-cycle.)  I'll leave it as a (hopefully easy) exercise; and of course you know how to check your answer!

<code>
&gt; generate cycles ([1..4] :: [Int])
[&lt;1,2,3,4&gt;,&lt;1,2,4,3&gt;,&lt;1,3,2,4&gt;,&lt;1,3,4,2&gt;,&lt;1,4,2,3&gt;,&lt;1,4,3,2&gt;]
</code>

As you can see, cycles are indicated with angle brackets; it is understood that <code>&lt;1,2,3,4&gt;</code>, <code>&lt;2,3,4,1&gt;</code>, <code>&lt;3,4,1,2&gt;</code>, and <code>&lt;4,1,2,3&gt;</code> are all equivalent.

At this point, you're probably thinking about a certain species and wondering why I haven't mentioned it yet---it seems pretty fundamental and primitive.  Are you thinking of... the species of <i>lists</i>?  It turns out that we don't have to take lists as primitive---we can define the species of lists as the <i>derivative</i> of the species of cycles!  <a href="http://strictlypositive.org/diff.pdf">The derivative of a regular type is its type of one-hole</a>... but I'm getting ahead of myself.  We'll look at species differentiation (along with several other operations on species) in the next post!

