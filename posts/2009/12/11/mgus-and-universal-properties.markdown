---
katex: true
title: "Mgu's and universal properties"
published: 2009-12-11T14:09:05Z
categories: math
tags: category theory,coequalizer,mgu,universal
---

Warning, poorly-explained categorical rambling follows...

The <i>most general unifier</i> (mgu) of two expressions $x$ and $y$ is a substitution $\theta$ for which $\theta(x) = \theta(y)$, such that every other substitution $\phi$ for which $\phi(x) = \phi(y)$ can be expressed as $\theta \circ \phi'$ for some $\phi'$.  For example, the most general unifier of $f(x,g(2))$ and $f(g(3), y)$ is $[x := g(3), y := g(2)]$.

Now, I'd seen this definition before.  But just yesterday I realized what a similar flavor it has to certain other definitions in mathematics.  For example:

<blockquote>
The <i>greatest common divisor</i> of two natural numbers $x$ and $y$ is a natural number $d$ which evenly divides both $x$ and $y$, such that every other common divisor of $x$ and $y$ is also a divisor of $d$.
</blockquote>

See the similarity?  Now, I've studied enough category theory to know that this is an example of a "universal property".  gcd in particular is the meet operation in the divisor poset, and meets are just products in a poset category...  so, naturally, I wondered whether mgu's could also be formalized as a particular universal construction in some category.  After some fiddling around, I figured out that yes, mgu's can be thought of as coequalizers in a category of substitutions (which I guess is sort of like the Kleisli category of the free monad on the structure functor for whatever term algebra you are using?).  A Google search for "mgu coequalizer" seems to suggest that I am right!  I'm rather pleased that I came up with this on my own.  Anyone know of a link to where this was first discussed?


