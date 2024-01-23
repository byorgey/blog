---
title: 'Polynomial Functors Constrained by Regular Expressions'
published: 2015-04-15T01:22:46Z
categories: math,writing
tags: differentiation,dissection,functor,matrices,polynomial,regular expression,semirings
---

I've now finished revising the paper that Dan Piponi and I had accepted to <a href="http://www.cs.ox.ac.uk/conferences/MPC2015/">MPC 2015</a>; you can find a PDF here:

<a href="http://www.cs.williams.edu/~byorgey/pub/type-matrices.pdf">Polynomial Functors Constrained by Regular Expressions</a>

Here's the 2-minute version: certain operations or restrictions on functors can be described by regular expressions, where the elements of the alphabet correspond to type arguments.  The idea is to restrict to only those structures for which an inorder traversal yields a sequence of types matching the regular expression. For example, $(aa)^*$ gives you even-size things; $a^*ha^*$ gives you the <a href="http://strictlypositive.org/diff.pdf">derivative</a> (the structure has a bunch of values of type $a$, a single hole of type $h$, and then more values of type $a$), and $b^*ha^*$ the <a href="http://strictlypositive.org/CJ.pdf">dissection</a>.

<a href="https://byorgey.files.wordpress.com/2015/04/dissected-tree.png"><img src="https://byorgey.files.wordpress.com/2015/04/dissected-tree.png?w=300" alt="dissected-tree" width="300" height="183" class="aligncenter size-medium wp-image-1431" /></a>

The punchline is that we show how to use the machinery of semirings, finite automata, and some basic matrix algebra to automatically derive an algebraic description of any functor constrained by any regular expression.  This gives a nice unified way to view differentiation and dissection; we also draw some connections to the theory of divided differences.

I'm still open to discussion, suggestions, typo fixes, etc., though at this point they won't make it into the proceedings.  There's certainly a lot more that could be said or ways this could be extended further.

