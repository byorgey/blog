---
title: Mgu's and universal properties
published: 2009-12-11T14:09:05Z
categories: math
tags: category theory,coequalizer,mgu,universal
---

Warning, poorly-explained categorical rambling follows...

The <i>most general unifier</i> (mgu) of two expressions $latex x$ and $latex y$ is a substitution $latex \theta$ for which $latex \theta(x) = \theta(y)$, such that every other substitution $latex \phi$ for which $latex \phi(x) = \phi(y)$ can be expressed as $latex \theta \circ \phi'$ for some $latex \phi'$.  For example, the most general unifier of $latex f(x,g(2))$ and $latex f(g(3), y)$ is $latex [x := g(3), y := g(2)]$.

Now, I'd seen this definition before.  But just yesterday I realized what a similar flavor it has to certain other definitions in mathematics.  For example:

<blockquote>
The <i>greatest common divisor</i> of two natural numbers $latex x$ and $latex y$ is a natural number $latex d$ which evenly divides both $latex x$ and $latex y$, such that every other common divisor of $latex x$ and $latex y$ is also a divisor of $latex d$.
</blockquote>

See the similarity?  Now, I've studied enough category theory to know that this is an example of a "universal property".  gcd in particular is the meet operation in the divisor poset, and meets are just products in a poset category...  so, naturally, I wondered whether mgu's could also be formalized as a particular universal construction in some category.  After some fiddling around, I figured out that yes, mgu's can be thought of as coequalizers in a category of substitutions (which I guess is sort of like the Kleisli category of the free monad on the structure functor for whatever term algebra you are using?).  A Google search for "mgu coequalizer" seems to suggest that I am right!  I'm rather pleased that I came up with this on my own.  Anyone know of a link to where this was first discussed?


