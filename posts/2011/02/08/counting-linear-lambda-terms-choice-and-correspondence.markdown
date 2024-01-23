---
title: Counting linear lambda terms: choice and correspondence
published: 2011-02-09T03:01:39Z
categories: combinatorics
tags: choice,inhabitation,isomorphism,lambda,linear
---

In <a href="http://byorgey.wordpress.com/2011/02/04/counting-linear-lambda-terms-mersenne-numbers/">my last post</a>, I showed how to write down polymorphic types with numbers of linear inhabitants given by products of factorials and Mersenne numbers, but left open the question of types with five linear inhabitants in particular, and whether in general it is possible to write down a type with any number of linear inhabitants.  The reason I was skeptical about the ability to have five inhabitants is that I didn't know of a straightforward way to encode <i>choice</i> -- the Mersenne and factorial constructions of course encoded some rather complicated choices.

However, in <a href="http://byorgey.wordpress.com/2011/02/04/counting-linear-lambda-terms-mersenne-numbers/#comment-8258">a comment on that post</a>, <a href="http://www.mathematik.uni-marburg.de/~rendel/">Tillmann Rendel</a> solved the remaining open questions by showing that straightforward choice is indeed possible.  Despite first appearances, it is possible to write down a polymorphic type with any desired number of linear inhabitants, in a completely uniform way.  Here is Tillmann's solution:

$latex \forall a. (a \to a) \to (a \to a, a \to a, \dots, a \to a)$

The number of linear inhabitants of this type is precisely the number of components in the output tuple.  Why is that?  Well, the input function must be put in one of the $latex n$ output slots (there's nothing else to do with it, since we have no values of type $latex a$ lying around to apply it to), and we must somehow fabricate functions of type $latex (a \to a)$ to go in the remaining slots.  By parametricity -- intuitively, since we know nothing about the type $latex a$ -- the only possibility is to use the identity function. However, the input function itself need not be the identity, since the caller gets to choose the type $latex a$; hence the caller can observe where we choose to route the input function.

Tillmann <a href="http://byorgey.wordpress.com/2011/02/04/counting-linear-lambda-terms-mersenne-numbers/#comment-8261">went on to remark</a>:

<blockquote>
So it seems that

$latex \varphi = \forall a. (a,a) \to a$

corresponds to

$latex \psi = \forall a. (a \to a) \to (a \to a, a \to a)$

in the sense that there is an isomorphism between the linear inhabitants of $latex \psi$ and the usual inhabitants of $latex \varphi$. This isomorphism is witnessed by the $latex \Lambda$-terms

$latex \lambda t. \lambda f. (t (f, \lambda x. x), t (\lambda x. x, f)) : \varphi \to \psi$

and

$latex \lambda t. \lambda (x,y). (\lambda(f,g). f x) (t (\lambda x. y)) : \psi \to \varphi.$

Can this observation be extended to yield an encoding of structural typing in linear typing and/or the other way around?

Such an encoding could play a similar role as double negation for classical and intuitionistic logics: $latex \tau$ has a classical proof if and only if $latex \neg (\neg \tau)$ has an intuitionistic proof.
</blockquote>

So we are looking for some sort of mapping from types to types such that there is an isomorphism between the linear inhabitants of one and the usual inhabitants of the other.  Actually, an <i>isomorphism</i> is rather strong: of course it makes sense to talk about isomorphisms since we have been talking about <i>counting</i>, but it would still be interesting even to have a mapping from types to types such that one is linearly inhabited (i.e. has at least one linear inhabitant) if and only if the other is inhabited in the usual way.  This would correspond more closely to the situation Tillmann alludes to with classical and intuitionistic logic, where we are usually unconcerned with the <i>number</i> of distinct proofs of a proposition, only whether or not it is provable.

The "obvious" transformation that suggests itself from Tillmann's solution (swap inputs for outputs and turn every occurrence of $latex a$ into $latex (a \to a)$) quickly disappoints as a candidate for inducing precise isomorphisms between linear and regular inhabitants.  For example, if we start with

$latex \forall a. a \to (a,a)$

(one usual inhabitant), we end up with

$latex \forall a. (a \to a, a \to a) \to (a \to a)$

which has two linear inhabitants (we can compose the functions in either order).  It doesn't even work in the more restricted sense explained above (simply preserving inhabitation):

$latex \forall a b. a \to (a,b \to b)$

has one inhabitant, but

$latex \forall a b. (a \to a, (b \to b) \to (b \to b)) \to (a \to a)$

has no linear inhabitants (since there is no way to use the input function involving $latex b$s).  But perhaps some variation on this theme will work.

Unfortunately, I don't know enough about the literature in this area to know whether anything like this has ever been explored before.  I'd be grateful for any pointers.

