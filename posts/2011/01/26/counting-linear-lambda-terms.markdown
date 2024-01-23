---
title: Counting linear lambda terms
published: 2011-01-26T18:03:50Z
categories: combinatorics
tags: lambda,linear,polymorphism,System F,terms
---

Just a little something with which I've been idly occupying spare brain cycles lately... read on for an interesting puzzle.

<h3>Warm up: counting lambda terms</h3>

Consider a stripped-down version of Haskell's type system with only natural numbers, polymorphism, functions, and tuples: no type classes, no algebraic data types, no base types other than Nat, and no bottom/undefined. (For those versed in the PL lingo, I mean System F extended with pairs -- or we could even Church-encode the pairs, it doesn't matter.)

Now for an easy warm-up exercise: for each of the following types, how <i>many different</i> terms are there with the given type?  By <i>different</i> I mean <i>observably different</i>; two terms are different if and only if there exist inputs for which they produce different outputs.  For example, $latex \lambda f. \lambda x. f x$ and $latex \lambda f. \lambda x. f (f x)$ are different, since they can be distinguished by passing them the arguments (say) $latex \lambda y. y + 1$ and $latex 0$.  However, $latex \lambda a. a$ and $latex \lambda b. (\lambda c. c) b$ are indistinguishable.  

Remember, you can only use lambda, function application, and tupling.  Using <code>undefined</code> in particular does not count.

<ol>
<li>$latex \forall a. a \to a$
<li>$latex \forall a. a \to a \to a$
<li>$latex \forall a. (a,a) \to (a,a)$
<li>$latex \forall a b. (a \to b) \to (a \to b) \to a \to a \to (b,b)$
<li>$latex \forall a. (a \to a) \to a \to a$
</ol>

The answers (stop reading if you want to work out the answers for yourself):

<ol>
	<li>One (the identity function).</li>
	<li>Two: we can return either the first or the second argument. (This is the type of Church-encoded booleans.)</li>
	<li>Four: return the first value twice, or the second value twice, or both in either order.</li>
	<li><strike>Eight: there are four ways to apply the two functions to the two values of type $latex a$ (apply one of them to both, or match them up one-to-one), and then two ways to order the results.</strike> Whoops, even I found this one tricky.  Thanks to gasche for the correct answer (sixteen); see the comments.</li>
	<li>Omega: we can apply the function to the second argument any natural number of times.</li>
</ol>

Hopefully you didn't find that too hard (although perhaps you found the fourth one a bit tricky).  Now for a few more (easy) warm-up questions (I won't bother giving the answers):

<ol>
	<li>Can you write down a type with exactly <i>three</i> different inhabitants?</li>
	<li>Can you come up with a scheme for constructing types inhabited by <i>any given natural number</i> of different terms?</li>
</ol>

<h3>Counting linear lambda terms</h3>

And now for the interesting twist.  We will now restrict ourselves to only <i>linear</i> lambda terms. By a "linear" term I mean, intuitively, one in which every input is used exactly once: inputs may be neither ignored nor duplicated.  Put another way, every time we see $latex \lambda x.t$, $latex x$ must occur free in $latex t$ exactly once.  (Note, however, that <i>type</i> arguments may be used multiple times.)  For example, $latex \lambda x. \lambda f. f x$ is linear, but $latex \lambda x. \lambda f. f (f x)$ is not (since $latex f$ is used twice), nor is $latex \lambda x. \lambda f. x$ (since $latex f$ is ignored).  For dealing with tuples, assume there are no projection functions like <code>fst</code> or <code>snd</code>, only pattern-matching like $latex \lambda (x,y). (y,x)$; hence, using a tuple linearly simply means using each of its components exactly once. We could make all of this fully precise with a suitable type system, of course, but I hope this intuitive explanation will suffice.

Now go back and look at the four types listed above.  How many <i>linear</i> inhabitants does each type have?

OK, that's slightly more interesting but still not that hard; hopefully you came up with one, zero, two, four, and one.  But now for the fun:

<ol>
	<li>Can you write down a type with exactly <i>three</i> different linear inhabitants?</li>
	<li>Can you come up with a scheme for constructing types inhabited by <i>any given natural number</i> of different linear terms?  If not, can you characterize the natural numbers for which such types do exist?</li>
</ol>

I'll write more in a future post.  For now, have fun, and feel free to post discussions, questions, solutions, etc. in the comments.

