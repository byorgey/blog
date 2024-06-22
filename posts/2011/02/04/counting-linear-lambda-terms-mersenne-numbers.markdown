---
katex: true
title: 'Counting linear lambda terms: Mersenne numbers'
published: 2011-02-04T20:32:42Z
categories: combinatorics
tags: 
---

In a <a href="https://byorgey.github.io/blog/posts/2011/01/26/counting-linear-lambda-terms.html">previous post</a> I posed the challenge of coming up with polymorphic types admitting certain numbers of linear inhabitants.  (If you didn't see the previous post and want to puzzle over an interesting lambda-calculus based problem, stop reading now and go read <a href="https://byorgey.github.io/blog/posts/2011/01/26/counting-linear-lambda-terms.html">the previous post</a> first.)  In this post I'll outline some solutions and some further questions.

First, we can make the easy observation that if we have polymorphic types admitting $m$ and $n$ linear inhabitants respectively, we can easily construct a type with $mn$ inhabitants, by first alpha-varying the types so they have disjoint type parameters, then concatenating their inputs and pairing their outputs.  For example, we can use $\forall a. (a,a) \to (a,a)$, with two linear inhabitants, to construct the type $\forall a b. (a,a) \to (b,b) \to ((a,a),(b,b))$ with four.

OK, so we can construct types with any power of two number of inhabitants.  In fact, we can get any factorial number of linear inhabitants with functions from n-tuples to n-tuples, whose linear inhabitants correspond to permutations.  For example, $\forall a. (a,a,a,a) \to (a,a,a,a)$ has $4! = 24$ linear inhabitants.  So now we can get any number which can be decomposed as a product of factorials.

But what about 3, or 5, or 7, or...?  After thinking for several days I finally stumbled upon this type:

$\forall a b. (a \to b) \to (b \to a) \to a \to b \to (a,b)$

which has exactly three linear inhabitants, namely
<ul>
	<li>$\lambda f g a b. (g (f a), b)$</li>
	<li>$\lambda f g a b. (g b, f a)$</li>
	<li>$\lambda f g a b. (a, f (g b))$</li>
</ul>

That is, we can pair up the function inputs with the other inputs, or we can apply both functions to one input, or both functions (in the other order) to the other input.  In each of these cases, the types ensure that we have no further choice -- for example, we can't swap the order of the outputs since then they would have the wrong types.

Generalizing a bit, consider the type

$\forall a_1, \dots, a_n. (a_1 \to a_2) \to \dots \to (a_{n-1} \to a_n) \to (a_n \to a_1) \\ \to a_1 \to \dots \to a_n \to (a_1,\dots,a_n)$

That is, we take as inputs $n$ functions arranged in a cycle (the first function maps from $a_1$ to $a_2$, the second from $a_2$ to $a_3$, and so on, with the $n$th function mapping from $a_n$ back to $a_1$), and $n$ inputs of types $a_1$ through $a_n$, and output an $n$-tuple of values with types $a_1$ to $a_n$.  How many linear inhabitants does this type have, as a function of $n$?

Well, if we compose all the functions together we can make them into a function $a_i \to a_i$ for any $i$, depending on the order in which we compose them.  So we can take all the functions and apply them to any one of the other inputs, passing through the remaining $n-1$ inputs unchanged.  We can also apply each function individually to its matching input, "shifting" the types of all the inputs by one.  Is there anything else we can do?

Indeed there is.  For example, we could chain together some of the functions to make a function (say) $a_2 \to a_7$, and if we compose the remaining functions we get a function $a_7 \to a_2$ -- so we can apply these functions to the inputs of type $a_2$ and $a_7$ respectively, swapping their positions in the output, and passing through the remaining inputs unchanged.  In fact, given a choice of <i>any</i> non-empty subset of the non-function inputs, there is exactly one way to use the given functions to cyclically permute them and pass through the other inputs unchanged.  For example, if we choose $a_1$, $a_3$, and $a_6$, then we must apply the first two functions to the $a_1$ to give an $a_3$, the next three functions to the $a_3$ which will give us an $a_6$, and the remaining functions applied to the $a_6$ will give us an $a_1$ again.

So the linear inhabitants of this type are in one-to-one correspondence with the non-empty subsets of $\{a_1, \dots, a_n\}$, which means there are $2^n - 1$ inhabitants.  This gives us 3, as we saw before, and it also tells us that

$\forall a b c. (a \to b) \to (b \to c) \to (c \to a) \to a \to b \to c \to (a,b,c)$

has 7 linear inhabitants.

Great, so now we can create arbitrary products of factorials and Mersenne numbers.  But what about, say, 5?

Well... I conjecture that no types with five linear inhabitants exist, but I don't have a proof yet!

