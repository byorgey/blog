---
katex: true
title: 'Typed type-level programming in Haskell, part III: I can haz typs plz?'
published: 2010-07-19T13:49:32Z
categories: haskell
tags: functional programming,type-level
---

In <a href="https://byorgey.github.io/blog/posts/2010/07/06/typed-type-level-programming-in-haskell-part-ii-type-families.html">Part II</a>, I showed how <i>type families</i> can be used to do type-level programming in a functional style.  For example, here is addition of natural numbers again:

<pre>
  data Z
  data S n

  type family Plus m n :: *
  type instance Plus Z n = n
  type instance Plus (S m) n = S (Plus m n)
</pre>

Now, <i>why</i> might we want to do such a thing?  One example (I know, I know, this is <i>always</i> the example... but hey, it's a good example) is if we wanted to have a type of polymorphic <i>length-indexed vectors</i> (or as they are sometimes known, "Length-Observed Lists") where the type of a vector includes its length.  Using a <a href="http://haskell.org/haskellwiki/GADT">generalized algebraic data type (GADT)</a>, we can write something like this:

<pre>
  data LOL :: * -&gt; * -&gt; * where
    KThxBye :: LOL Z a
    Moar    :: a -&gt; LOL n a -&gt; LOL (S n) a
</pre>

This says that
<ol>
	<li><code>LOL</code> is a type constructor of kind <code>* -&gt; * -&gt; *</code>, that is, it takes two type arguments of kind <code>*</code> and produces a type of kind <code>*</code>. The intention is that the first argument records the length, and the second records the type of the elements.</li>
	<li><code>KThxBye</code> constructs a vector of length zero.</li>
	<li>Given an element of type <code>a</code> and a vector of </code>a</code>s of length <code>n</code>, <code>Moar</code> constructors a vector of length <code>S n</code>.</li>
</ol>

The type-level function <code>Plus</code> comes in when we implement an <code>append</code> function for our length-indexed vectors: in order to express the type of <code>append</code> we have to add the lengths of the input vectors.

<pre>
  append :: LOL m a -&gt; LOL n a -&gt; LOL (Plus m n) a
  append KThxBye     v = v
  append (Moar x xs) v = Moar x (append xs v)
</pre>

If you haven't already seen things like this, it's a good exercise to figure out why this definition of <code>append</code> typechecks (and why it <i>wouldn't</i> typecheck if we put anything other than <code>Plus m n</code> as the length of the output).

OK, great!  We can make GHC check the lengths of our lists at compile time.  So what's the problem?  Well, there are (at least) three obvious things which this code leaves to be desired:
<ol>
	<li>It doesn't matter whether we have already declared a <code>Nat</code> type with constructors <code>Z</code> and <code>S</code>; we have to redeclare some empty types <code>Z</code> and <code>S</code> to represent our type-level natural number "values".  And declaring empty types to use like "values" seems silly anyway.</li>
	<li>It also doesn't matter whether we've already implemented a <code>plus</code> function for our <code>Nat</code> values; we must re-code the addition algorithm at the type level with the type family <code>Plus</code>. Especially irksome is the fact that these definitions will be virtually identical.</li>
	<li>Finally, and most insidiously, <code>LOL</code> is essentially <i>untyped</i>.  Look again at the kind of <code>LOL :: * -&gt; * -&gt; *</code>.  There's nothing in the kind of <code>LOL</code> that tells us the first argument is supposed to be a type-level number.  Nothing prevents us from accidentally writing the type <code>LOL Int (S Z)</code> -- we'll only run into (potentially confusing) problems later when we try to write down a value with this type.</li>
</ol>

Wouldn't it be nice if we could reuse (1) values and (2) functions at the type level, and (3) get more informative kinds in the bargain?  Indeed, inspired by Conor McBride's <a href="http://personal.cis.strath.ac.uk/~conor/pub/she/">SHE</a>, our work aims precisely to enable (1) and (3) in GHC as a start, and hopefully eventually (2) (and other features) as well.  Hopefully soon, you'll be able to write this:

<pre>
  data Nat = Z | S Nat

  type family Plus (m::Nat) (n::Nat) :: Nat
  type instance Plus Z n = n
  type instance Plus (S m) n = S (Plus m n)

  data LOL :: Nat -&gt; * -&gt; * where
    KThxBye :: LOL Z a
    Moar    :: a -&gt; LOL n a -&gt; LOL (S n) a

  append :: ...  -- exactly the same as before
</pre>

...or even this:

<pre>
  data Nat = Z | S Nat

  plus :: Nat -&gt; Nat -&gt; Nat
  plus Z n = n
  plus (S m) n = S (plus m n)

  data LOL :: Nat -&gt; * -&gt; * where ... -- same as above

  append :: LOL m a -&gt; LOL n a -&gt; LOL (plus m n) a
  append = ...  -- same as before
</pre>

In another post I'll explain what the above fantasy code would be doing in a bit more detail, talk about precisely how we propose to accomplish this, and discuss why we might want to do things this way, rather than introducing full dependent types (or just chucking Haskell and all moving to <a href="http://wiki.portal.chalmers.se/agda/pmwiki.php">Agda</a>).


