---
title: 'Typed type-level programming in Haskell, part IV: collapsing types and kinds'
published: 2010-08-05T14:28:28Z
categories: haskell,projects
tags: lifting,programming,type-level,types
---

In <a href="http://byorgey.wordpress.com/2010/07/19/typed-type-level-programming-in-haskell-part-iii-i-can-haz-typs-plz/">Part III</a>, we saw how the current state of the art in Haskell type-level programming leaves some things to be desired: it requires duplicating both data declarations and code, and even worse, it's untyped.  What to do?

Currently, GHC's core language has three "levels":
<ol>
	<li>Expressions: these include things like term variables, lambdas, applications, and case expressions.</li>
	<li>Types: e.g. type variables, base types, forall, function types.</li>
	<li>Kinds: <code>*</code> and arrow kinds.</li>
</ol>

Types classify expressions (the compiler accepts only well-typed expressions), and kinds classify types (the compiler accepts only well-kinded types).  For example,

<p><code>3 :: Int</code>,<br />
<code>Int :: *</code>,<br />
<code>Maybe :: * -&gt; *</code>, 
</p>

and so on.

The basic idea is to allow the automatic lifting of types to kinds, and their data constructors to type constructors.  For example, assuming again that we have
<pre>
data Nat = Z | S Nat
</pre>
we can view <code>Z :: Nat</code> as either the <i>data constructor</i> <code>Z</code> with <i>type</i> <code>Nat</code>, or as the <i>type</i> <code>Z</code> with <i>kind</i> <code>Nat</code>. Likewise, <code>S :: Nat -&gt; Nat</code> can be viewed either as a data constructor and a type, or a type constructor and a kind.

One obvious question: if <code>Z</code> is a type, and types classify expressions, then what expressions have type <code>Z</code>?  The answer: there aren't any.  But this makes sense. We want to be able to use <code>Z</code> as a type-level "value", and don't really care about whether it classifies any expressions.  And indeed, without this auto-lifting, if we wanted to have a type-level <code>Z</code> we would have declared an <i>empty</i> data type <code>data Z</code>.

Notice we have much richer kinds now, since we are basically importing an entire copy of the type level into the kind level.  But that also means we will need something to classify kinds as well, so we need another level... and what's to stop us from lifting kinds up to the next level, and so on?  We would end up with an infinite hierarchy of levels.  In fact, this is exactly what <a href="http://code.google.com/p/omega/">Omega</a> does.  

But in our case we can do something much simpler: we simply <i>collapse</i> the type and kind levels into a single level so that types and kinds are now the same thing, which I will call typekinds (for lack of a better term).  We just take the ordinary syntax of types that we already had, and the only things we need to add are lifted data constructors and <code>*</code>.  (There are still some questions about whether we represent arrow kinds using the arrow type constructor or forall, but I'll leave that aside for the moment.)  To tie the knot, we add the axiom that the typekind <code>*</code> is classified by itself.  It is well-known that this allows the encoding of set-theoretic paradoxes that render the type system inconsistent when viewed as a logic -- but Haskell's type system is already an inconsistent logic anyway, because of general recursion, so who cares?

So, what are the difficult issues remaining?
<ul>
	<li>Coercions: GHC's core language includes a syntax of coercions for explicitly casting between equivalent types.  Making the type system richer requires more sophisticated types of coercions and makes it harder to prove that everything still works out.  But I think we have this mostly ironed out.</li>
	<li>Surface syntax: Suppose in addition to <code>Z :: Nat</code> we also declare a type called <code>Z</code>.  This is legal, since expressions and types inhabit different namespaces.  But now suppose GHC sees <code>Z</code> in a type.  How does it know which <code>Z</code> we want?  Is it the type <code>Z</code>, or is it the data constructor <code>Z</code> lifted to a type?  There has to be a way for the programmer to specify what they mean.  I think we have a solution to this that is simple to understand and not too heavyweight -- I can write about this in more detail if anyone wants.</li>
	<li>Type inference: This probably makes type inference a lot harder.  But I wouldn't know for sure since that's the one thing we haven't really thought too hard about yet.</li>
</ul>

One final question that may be bothering some: why not just go all the way and collapse <i>all</i> the levels, and have a true dependently-typed language?  It's a valid question, and there are of course languages, notably Agda, Coq, and Epigram, which take this approach.  However, one benefit of maintaining a separation between the expression and typekind levels is that it enables a simple <i>erasure semantics</i>: everything at the expression level is needed at runtime, and everything at the typekind level can be erased at compile time since it has no <i>computational</i> significance.  Erasure analysis for languages with collapsed expression and type levels is still very much an active area of research.

There's more to say, but at this point it's probably easiest to just open things up to questions/comments/feature requests and I'll write more about whatever comes up!  I should probably give more examples as well, which I'll try to do soon.

