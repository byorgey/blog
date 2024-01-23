---
title: Higher-dimensional enumeration
published: 2007-10-01T21:27:08Z
categories: combinatorics,haskell
tags: binomial,enumeration,generalization,non-regular types,simplex
---

The other day in the <a href="http://www.haskell.org/haskellwiki/IRC_channel">#haskell IRC channel</a>, some of us were playing around with generalizing Haskell list enumeration syntax:
<pre>
Prelude&gt; [1..5]
[1,2,3,4,5]</pre>
First, we defined the (...) operator to perform enumeration, so it can be partially applied.  (Unfortunately, we can't define a (..) operator for this purpose, since it's reserved syntax.)
<pre>
Prelude&gt; let x ... y = [x..y]
Prelude&gt; map (...4) [1,3,7]
[[1,2,3,4],[3,4],[]]</pre>
So, how to generalize?  Someone typed this:
<pre>
Prelude&gt; let x .... y = map (x...) (x...y)
Prelude&gt; 1 .... 3
[[1],[1,2],[1,2,3]]</pre>
Interesting!  And what about five dots?  Unfortunately, this doesn't work:
<pre>
Prelude&gt; let x ..... y = map (x...) (x....y)

&lt;interactive&gt;:1:28:
    Occurs check: cannot construct the infinite type: a = [a]
      Expected type: [a]
      Inferred type: [[a]]
    In the second argument of `map', namely `(x .... y)'
    In the expression: map ((x ...)) (x .... y)</pre>
Of course, the problem is that (x .... y) has type [[a]], so we can't just use map (x...), we have to map twice in order to reach the proper depth:
<pre>
Prelude&gt; let x ..... y = map (map (x...)) (x .... y)
Prelude&gt; 1 ..... 3
[[[1]],[[1],[1,2]],[[1],[1,2],[1,2,3]]]</pre>
We could keep going, although it gets increasingly difficult to tell apart the number of dots.
<pre>
x ...... y = (map . map . map) (x...) (x ..... y)
x ....... y = (map . map . map . map) (x...) (x ...... y)
...</pre>
Let's take a step back and think about what is going on here.  First, (1...3) just means we are counting from 1 up to 3: [1,2,3].  At the next level, (1 <tt>....</tt> 3) means we are counting up to (1 ... 3) = [1,2,3]:  first [1], then [1,2], then [1,2,3].  So (1 <tt>....</tt> 3) = [[1],[1,2],[1,2,3]].  We can illustrate it thus:
<pre>
    1
  1    2
1    2    3</pre>
Then (1 <tt>.....</tt> 3) means we're counting up to THAT: first [[1]], then [[1],[1,2]], then [[1],[1,2],[1,2,3]].  Any guesses on a nice visualization?  That's right, it's a tetrahedron!

In general, continuing this procedure gives us higher- and higher-dimensional <a href="http://mathworld.wolfram.com/Simplex.html">simplices</a>.  From this perspective, an expression like [1..5] is really giving us discrete points on a 1-simplex, i.e. a line.  Now, the big question: can we generalize the operators (...), (....), (.....), etc. into a single higher-order function which can compute this "simplicial enumeration of order n", for any given n &gt; 0?

At first blush this doesn't seem possible.  For one thing, there's a problem with assigning a type to this hypothetical higher-order enumeration function: each of the enumeration operators we made returns a different type!  (...) returns [a], (<tt>....</tt>) returns [[a]], and so on.  So we'd have to unify the infinite family of types [a], [[a]], [[[a]]], etc. into a single type.  Secondly, there's the problem that each specific enumeration operator uses a different number of calls to map in its implementation.  We could try to copy and paste <a href="http://okmij.org/ftp/Haskell/typecast.html">some code from Oleg</a> involving overlapping instances and crazy typeclasses... but (in this instance) there's a better way.

What we'd like to do is create a type which encompasses [a], [[a]], [[[a]]], and so on.  A good first try might be:
<pre>
data DeepList a = List [a] | Deep [DeepList a]</pre>
which says that a DeepList of 'a' is either a list of 'a', or a list of DeepLists of 'a'.  Sounds good, right?  Unfortunately, there is a subtle problem with this definition: lists in different parts of a DeepList value may have different depths.  For example, the following is a perfectly legitimate value of type DeepList Int:
<pre>
Deep [List [1,2,3], Deep [List [1,6], List [4]]]</pre>
If we remove the Deep and List constructors, however, this corresponds to the ill-typed list value [[1,2,3], [[1,6], [4]]].  Since we only want values corresponding to nested list types, this ability to have different-depth lists in different parts of the tree won't do.  How can we force different branches to all have the same depth?

To the rescue come <a href="http://citeseer.ist.psu.edu/370027.html">non-regular types</a> -- that is, recursive parameterized types where the type parameters on the right side are not the same as on the left.  In particular, we can use a Bush type defined as follows:
<pre>
data Bush a = Leaves [a] | Trunk (Bush [a])</pre>
This definition is very similar to that of DeepList, but the key difference is that a Bush may consist of a Bush of lists of 'a', rather than a list of Bushes of 'a'.  Every occurrence of the Trunk constructor adds one more nested layer to the type parameter, until finally a Leaves constructor is reached, followed by a suitably-nested list.  For example, here are some values of type (Bush Int):
<pre>
Leaves [1,2,3]
Trunk (Leaves [[1,2], [6,7]])
Trunk (Trunk (Leaves [[[5],[6,8,9],[]],[[6,19],[20]]]))</pre>
You can check that something like Trunk (Leaves [1,2,3]) gives a type error.

So, our higher-order enumeration function can return a Bush.  But how will we deal with the (map . map ...)'s?  Actually, this is easier than it looks: all we need to do is make Bush a Functor, since that's really what's going on: we just want to be able to map (x...) over the result of the enumeration one dimension lower, whatever that might happen to mean.  So, here we go:
<pre>
import Data.List

data Bush a = Trunk (Bush [a]) | Leaves [a]

instance (Show a) =&gt; Show (Bush a) where   -- show Bush values without the constructors
  show (Leaves l) = show l
  show (Trunk b)  = show b

instance Functor Bush where
  fmap f (Trunk b)  = Trunk $ fmap (map f) b    -- add another map
  fmap f (Leaves l) = Leaves $ map f l

flatten :: Bush a -&gt; [a]                  -- flatten a Bush into a single list
flatten (Leaves l) = l
flatten (Trunk b)  = concat $ flatten b

x ... y = [x..y]</pre>
And now our higher-order enumeration function (which we'll call simplex, for reasons which are hopefully clear) is easy to write:
<pre>
simplex :: (Enum t, Integral a) =&gt; a -&gt; t -&gt; t -&gt; Bush t
simplex n _ _ | n &lt; 1 = Leaves []
simplex 1 x y = Leaves (x...y)
simplex n x y = Trunk $ fmap (x...) (simplex (n-1) x y)</pre>
Let's try it out:
<pre>
*Main&gt; simplex 1 1 3
[1,2,3]
*Main&gt; simplex 2 1 3
[[1],[1,2],[1,2,3]]
*Main&gt; simplex 3 1 3
[[[1]],[[1],[1,2]],[[1],[1,2],[1,2,3]]]</pre>
It works!  Finally, I'll leave the explanation of the following QuickCheck properties as an exercise for the reader:
<pre>
simplex' :: (Integral a, Integral t) =&gt; a -&gt; t -&gt; Bush t
simplex' n x = simplex n 1 x

fac :: (Integral t) =&gt; t -&gt; t
fac n = product [1..n]
binom :: Integer -&gt; Integer -&gt; Integer
binom n k | n &lt; k     = 0
          | k &lt; 0     = 0
          | otherwise = (fac n) `div` (fac k * fac (n-k))

prop_simplex k n = (k &gt; 0) ==&gt;
    (genericLength . flatten . simplex' k $ n) == binom (n + k - 1) k

prop_simplex_components k n = (k &gt; 0) ==&gt;
    (map genericLength . group . sort . flatten . simplex' k $ n) == map (flip binom (k-1)) [k+n-2,k+n-3..(k-1)]</pre>

