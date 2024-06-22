---
katex: true
title: 'Typed type-level programming in Haskell, part II: type families'
published: 2010-07-06T10:28:37Z
categories: haskell
tags: programming,type families,type-level,types
---

In my <a href="https://byorgey.github.io/blog/posts/2010/06/29/typed-type-level-programming-in-haskell-part-i-functional-dependencies.html">previous post</a>, we saw how multi-parameter type classes with functional dependencies in Haskell allow us to do type-level programming in a logic programming style.  (If you're not clear on why this corresponds to a logic programming style, see the <a href="http://www.reddit.com/r/haskell/comments/ck459/typed_typelevel_programming_in_haskell_part_i/">ensuing discussion on reddit</a>, where others explained it much better than I did in my post.)

However, MPTCs + FDs weren't the last word on type-level programming.  In 2007, along came <a href="http://www.haskell.org/haskellwiki/GHC/Type_families">type families</a>.

Essentially, type families allow us to write <i>functions on types</i>.  For example, here's how we would implement the same <code>Plus</code> function from the last post, this time using type families:

<pre>
  data Z
  data S n

  type family Plus m n :: *
  type instance Plus Z n = n
  type instance Plus (S m) n = S (Plus m n)
</pre>

This says that for any types <code>m</code> and <code>n</code>, <code>Plus m n</code> is  type of kind <code>*</code>.  But it isn't a <i>new</i> type, it's just an alias for some existing type.  It's instructive to think carefully about the difference between this and type synonyms.  After all, using a type synonym declaration, we can already make <code>Plus m n</code> an alias for some existing type, right?

Well, yes, but the difference is that a type synonym <i>doesn't get to look at its arguments</i>.  The technical term for this is that type synonyms must be <i>parametric</i>.  So, for example, we can say

<pre>
  type Foo m n = [(m, Maybe n)]
</pre>

which defines the type synonym <code>Foo</code> uniformly for all arguments <code>m</code> and <code>n</code>, but using only type synonyms we <i>cannot</i> say

<pre>
  type Foo m Int = [m]
  type Foo m Char = Maybe m
</pre>

where <code>Foo</code> acts differently depending on what its second argument is.  However, this is precisely what type families allow us to do -- to declare type synonyms that do pattern-matching on their type arguments.  Looking back at the <code>Plus</code> example above, we can see that it evaluates to different types depending on whether its first argument is <code>Z</code> or <code>S n</code>. Notice also that it is essentially identical to the way we would implement addition on regular value-level natural numbers, using pattern-matching on the first argument and a recursive call in the successor case:

<pre>
  data Nat = Z | S Nat

  plus :: Nat -&gt; Nat -&gt; Nat
  plus Z n = n
  plus (S m) n = S (plus m n)
</pre>

Let's check that <code>Plus</code> works as advertised:

<pre>
  *Main&gt; :t undefined :: Plus (S Z) (S Z)
  undefined :: Plus (S Z) (S Z) :: Plus (S Z) (S Z)
</pre>

Well, unfortunately, as a minor technical point, we can see from the above that ghci doesn't expand the type family for us.  The only way I currently know how to force it to expand the type family is to generate a suitable error message:

<pre>
  *Main&gt; undefined :: Plus (S Z) (S Z)

  ...No instance for (Show (S (S Z)))...
</pre>

This is ugly, but it works: <code>S (S Z)</code> is the reduced form of <code>Plus (S Z) (S Z)</code>.

So type families let us program in a <i>functional</i> style.  This is nice -- I daresay most Haskell programmers will be more comfortable only having to use a single coding style for both the value level and the type level.  There are a few cases where a logic programming style can be quite convenient (for example, with an additional functional dependency we can use the <code>Plus</code> type class from the last post to compute both addition <i>and</i> subtraction), but in my opinion, the functional style is a huge win in most cases.  (And, don't worry, FDs and TFs are <a href="http://www.haskell.org/pipermail/haskell-cafe/2009-February/055890.html">equivalent in expressiveness</a>.)

Of course, there is a lot more to all of this; for example, I haven't even mentioned data families or associated types.  For more, I recommend reading the excellent <a href="http://www.haskell.org/haskellwiki/Simonpj/Talk:FunWithTypeFuns">tutorial</a> by Oleg Kiselyov, Ken Shan, and Simon Peyton Jones, or the page on the <a href="http://www.haskell.org/haskellwiki/GHC/Type_families">GHC wiki</a>.  For full technical details, you can look at the <a href="http://www.cse.unsw.edu.au/~chak/papers/SCPD07.html">System FC paper</a>.

Nothing is ever perfect, though --- in my next post, I'll explain what type families still leave to be desired, and what we're doing to improve things.

