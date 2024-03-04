---
katex: true
title: 'Primitive species and species operations'
published: 2009-07-30T21:48:32Z
categories: combinatorics,haskell,math
tags: combinatorial species,DSL,primitives
---

In this second post about my new <a href="http://hackage.haskell.org/package/species">combinatorial species library</a>, I plan to begin writing about the species DSL itself: what are the primitive combinatorial species and the primitive operations on species?  (The <a href="http://byorgey.wordpress.com/2009/07/24/introducing-math-combinatorics-species/">first post described the concept of combinatorial species in general</a>.  Also, for those following along at home, I've just uploaded version 0.2.1 of the species library, which is a vast improvement over 0.1, with many new features and a few bug fixes; just <code>cabal update &amp;&amp; cabal upgrade species</code>.  Also also, note that it currently only builds on ghc 6.10.x.)

<h3>The Species type class</h3>

The central point of the combinatorial species formalism is that there is a deep and fruitful <i>analogy</i> between species and certain types of generating functions: every species corresponds to (several different types of) generating functions, and every species operation corresponds, in a fairly natural way, to operations on generating functions.  For example, "adding" two species has the combinatorial interpretation of disjoint sum, but also corresponds to generating function addition---that is, the generating function of a sum of species is the sum of their generating functions.  Like every good generating function, these generating functions encode various sorts of information about species (such as counting labelled or unlabelled structures), so once we have written down a description of a combinatorial structure using primitive species and species operations, we can use the generating function analogy to compute various properties of the species.

So, how to represent combinatorial species in our library?  With a <code>Species</code> type class, of course!  The type class mechanism is perfectly suited to this situation---we have an abstract algebraic structure (species and species operations) which can be interpreted in several ways.  So we can write down an expression of type <code>Species s =&gt; s</code>, and then choose to compute certain things about it simply by choosing what type it should be.  Without further ado, let's see (an idealized version of) the type class itself, defined in <code>Math.Combinatorics.Species.Class</code>:

<pre>
class (Algebra.Differential.C s) =&gt; Species s where
  singleton :: s
  set       :: s
  cycle     :: s

  o         :: s -&gt; s -&gt; s
  cartesian :: s -&gt; s -&gt; s
  fcomp     :: s -&gt; s -&gt; s
  ofSize    :: s -&gt; (Integer -&gt; Bool) -&gt; s
</pre>

(I've actually left out a few methods, but they all share the property that they have default implementations in terms of the other methods, and are only in the class so they can be given specialized implementations in certain instances.  I've left them out for now to simplify the discussion.)

So we can now write expressions like

<code>(set `ofSize` (&gt;3)) `fcomp` (cycle `o` singleton) :: Species s =&gt; s</code>

but what does it mean?  (Actually, this particular example is pretty meaningless. =)  And what's that <code>Algebra.Differential.C</code> constraint?  Let's start at the beginning.

<h3>0</h3>

The <code>Algebra.Differential.C</code> constraint requires any instance of <code>Species</code> to be a <i>differentiable ring</i>.  In particular, it (transitively) implies the constraint <code>Algebra.Additive.C</code>, which means that instances of <code>Species</code> must form an additive group: there must be a species operation <code>(+)</code>, and a species <code>0</code> which is the identity for <code>(+)</code>.  (It also requires an operation <code>negate</code> which produces additive inverses, but that isn't implemented yet!) Let's see what these correspond to.

The species $0$ is the Scrooge of the species world: it refuses to create a single structure, no matter how many labels you give it!

[caption id="attachment_220" align="aligncenter" width="400" caption="The species 0"]<img src="http://byorgey.files.wordpress.com/2009/07/zero.png" alt="The species 0" title="zero" width="400" height="92" class="size-full wp-image-220" />[/caption]

Let's see how to use this species with the library:
<code>
&gt; take 10 $ labelled 0
[0,0,0,0,0,0,0,0,0,0]
&gt; take 10 $ unlabelled 0
[0,0,0,0,0,0,0,0,0,0]
&gt; generate 0 ([1..3] :: [Int])
[]
</code>

Pretty boring, huh?  Well, it's supposed to be.  $0$ doesn't get explicitly used very much, but it's nice to know it's there.

(Also, remember that to follow along, you'll have to start ghci with the <code>-XNoImplicitPrelude</code> flag, then remove the loaded Prelude module with <code>:m -Prelude</code>, and then load <code>MyPrelude</code> (from the NumericPrelude library) and the species library: <code>:m +MyPrelude Math.Combinatorics.Species</code>.)

<h3>Species sum</h3>

And what about species addition?  Addition just corresponds to disjoint (i.e. tagged) union: an $(F+G)$-structure is either an $F$-structure or a $G$-structure, along with a tag so you know which it is.  If you have $m$ $F$-structures and $n$ $G$-structures, then you have $m + n$ $(F+G)$-structures.

<code>
&gt; take 10 $ labelled lists
[1,1,2,6,24,120,720,5040,40320,362880]
&gt; take 10 $ labelled octopi
[0,1,3,14,90,744,7560,91440,1285200,20603520]
&gt; take 10 $ labelled (lists + octopi)
[1,2,5,20,114,864,8280,96480,1325520,20966400]
&gt; generate (lists + octopi) ([1,2] :: [Int])
[inl([1,2]),inl([2,1]),inr(&lt;[1,2]&gt;),inr(&lt;[2,1]&gt;),
 inr(&lt;[1],[2]&gt;)]
</code>

Do you see why the $0$ species is the identity element for species sum?  If you have a structure of the species $0 + F$, it must be either a $0$-structure, or an $F$-structure: but there <i>are</i> no $0$-structures!  Now, you may complain that $0$ is not really an <i>identity</i>, since the addition still introduces an extra tag:

<code>
&gt; generate subsets ([1..3] :: [Int])
[{1,2,3},{1,2},{1,3},{1},{2,3},{2},{3},{}]
&gt; generate (0 + subsets) ([1..3] :: [Int])
[inr({1,2,3}),inr({1,2}),inr({1,3}),inr({1}),
 inr({2,3}),inr({2}),inr({3}),inr({})]
</code>

That's true, but we really only care about species identity <i>up to isomorphism</i>, and the species $F$, $0 + F$, and $F + 0$ are clearly all isomorphic for any species $F$, even if they are not identical.

<h3>1</h3>

The <code>Algebra.Differential.C</code> constraint also implies a <code>Algebra.Ring.C</code> constraint, which requires a multiplication operation <code>(*)</code> and identity element <code>1</code>.

So, what is the species $1$?  It puts a singleton structure on the empty set of labels, but no structures on any nonempty label sets:

[caption id="attachment_223" align="aligncenter" width="400" caption="The species 1"]<img src="http://byorgey.files.wordpress.com/2009/07/one.png" alt="The species 1" title="one" width="400" height="198" class="size-full wp-image-223" />[/caption]

<code>
&gt; take 10 $ labelled 1
[1,0,0,0,0,0,0,0,0,0]
&gt; take 10 $ unlabelled 1
[1,0,0,0,0,0,0,0,0,0]
&gt; generate 1 ([] :: [Int])
[1]
&gt; generate 1 ([1..3] :: [Int])
[]
</code>

So you can see that on the empty set, $1$ generates a single structure which is also called 1 (although it could be called anything, really).

<h3>Species product</h3>

And species product?  An $(F*G)$-structure on a set of labels is a pair consisting of an $F$-structure on a subset of the labels, and a $G$-structure on whatever labels are left over.  In other words, to form all $(F*G)$-structures on a set of labels $U$, we first partition $U$ into an ordered pair of subsets in all possible ways, and for each pair, take all possible combinations of an $F$-structure on the first subset, and a $G$-structure on the second subset.  For example:

<code>
&gt; generate (list * list) ([1..3] :: [Int])
[([1,2,3],[]),([1,3,2],[]),([2,1,3],[]),([2,3,1],[]),([3,1,2],[]),([3,2,1],[]),([1,2],[3]),([2,1],[3]),([1,3],[2]),([3,1],[2]),([1],[2,3]),([1],[3,2]),([2,3],[1]),([3,2],[1]),([2],[1,3]),([2],[3,1]),([3],[1,2]),([3],[2,1]),([],[1,2,3]),([],[1,3,2]),([],[2,1,3]),([],[2,3,1]),([],[3,1,2]),([],[3,2,1])]
</code>

Can you see why $1$ is the identity element for this operation?  The only partition of the label set that will produce any $(1*F)$-structures is $(\emptyset, U)$: in any other case, $1$ produces no structures.  But a $1$-structure paired with an $F$-structure on $U$ is really just an $F$-structure on $U$, since there is only one $1$-structure.

As an exercise, can you figure out what the species $2$, $3$, ... ought to be?

I think I'll stop there for now.  In my next post, I'll talk about the other primitive species in the <code>Species</code> type class: singletons, sets, and cycles.

