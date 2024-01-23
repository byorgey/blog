---
title: List convolutions
published: 2008-04-22T20:23:35Z
categories: haskell
tags: convolution,list,power series
---

On the #haskell IRC channel a week or so ago, cdsmithus asked:

<blockquote>
An easy (hopefully) question.  I have an infinite list.  How do I get a list of ordered pairs of stuff from the first list?
</blockquote>

Several people suggested variations on the following theme:

<pre>
pairs = [(b,a-b) | a &lt;- [0..], b &lt;- [0..a]]
</pre>

which produces the list <code>pairs = [(0,0), (0,1), (1,0), (0,2), (1,1), (2,0), (0,3), (1,2), (2,1), (3,0), ...</code> I'm not sure if this was exactly the solution cdsmithus wanted, but just to humor me, let's suppose it was. =)  In a sense, this can be considered a "universal" sort of solution, since given any infinite list <code>xs :: [a]</code>, we can get a corresponding list of pairs from <code>xs</code> by evaluating

<pre>
map (\(a,b) -&gt; (xs!!a, xs!!b)) pairs
</pre>

But of course this isn't very efficient, since <code>(xs!!a)</code> is O(a) -- the beginning of <code>xs</code> is getting unnecessarily traversed over and over again. So, can we do this in a more "direct" way?

Notice the similarity to power series multiplication: multiplying the general power series $latex (a_0 + a_1x_1 + a_2x^2 + \dots)$ and $latex (b_0 + b_1x_1 + b_2x^2 + \dots)$ yields

$latex a_0b_0 + (a_0b_1 + a_1b_0)x_1 + (a_0b_2 + a_1b_1 + a_2b_0)x^2 + \dots$

Both are instances of <a href="https://secure.wikimedia.org/wikipedia/en/wiki/Convolution#Discrete_convolution">discrete convolution</a>.  I immediately thought of <a href="http://www.cs.dartmouth.edu/~doug/">Douglas McIlroy's</a> classic Functional Pearl, <a href="http://citeseer.ist.psu.edu/101898.html">Power Series, Power Serious</a>.  In it, he exhibits some amazing, simple, elegant Haskell code for computing with power series, treated as infinite (lazy!) lists of coefficients.  The part we're interested in is the definition of lazy power series multiplication:

<pre>
(f:fs) * (g:gs) = f*g : (f.*gs + g.*fs + (0 : fs*gs))
</pre>

where <code>x .* ys = map (x*) ys</code>, essentially.  This code may seem mysterious, but any confusion is quickly cleared up by the simple algebraic derivation:

$latex (f + xF_1) \times (g + xG_1) = fg + x(fG_1 + gF_1 + x(F_1 \times G_1))$

So, can we adapt this to compute list convolutions instead of numeric power series convolutions? Sure!  We just need to make a few adjustments.  First, we'll replace element-wise multiplication with the tupling operator <code>(,)</code>.  And instead of addition, we'll use list concatenation to collect tupled results.  Finally, since tupling and concatenation are not commutative like multiplication and addition, we'll have to be a bit more careful about order.  There are a few other minor issues, but I'll just let the code speak for itself:

<pre>
import Prelude hiding ((+),(*),(**))
import qualified Prelude as P

(+) = zipWith (++)
x * y = [(x,y)]
x .* ys = map (x*) ys
ys *. x = map (*x) ys

(**) :: [a] -&gt; [b] -&gt; [[(a,b)]]
[]     ** _      = []
_      ** []     = []
(x:xs) ** (y:ys) = x*y : (x .* ys) + ([] : (xs ** ys)) + (xs *. y)
</pre>

We can test it out in ghci (being sure to pass ghci the <code>-fno-implicit-prelude</code> option so we don't get conflicts with our definition of <code>(**)</code>):

<pre>
&gt; take 10 . concat $ [1..] ** ['a'..]
[(1,'a'),(1,'b'),(2,'a'),(1,'c'),(2,'b'),(3,'a'),(1,'d'),(2,'c'),(3,'b'),(4,'a')]
&gt; take 10 . concat $ [0..] ** [0..]
[(0,0),(0,1),(1,0),(0,2),(1,1),(2,0),(0,3),(1,2),(2,1),(3,0)]
</pre>

Cool!  Now, there's just one issue left: this code is still rather slow, because of the way it uses list concatenation repeatedly to accumulate results; in fact, I suspect that it ends up being not much better, speed-wise, than the naive code we looked at first which generates numeric tuples and then indexes into the lists! Taking the first one million elements of <code>concat $ [1..] ** [1..]</code>, multiplying each pair, and summing the results takes around 16 seconds on my machine.

We can easily fix this up by using "difference lists" instead of normal lists: we represent a list <code>xs</code> by the function <code>(xs++)</code>.  Then list concatenation is just function composition -- O(1) instead of O(n).  Kenn Knowles <a href="http://www.kennknowles.com/blog/2008/04/16/drawing-fractals-in-haskell-with-a-cursor-graphics-dsel-and-a-cute-list-representation/">wrote about this representation recently</a>, and Don Stewart has written <a href="http://hackage.haskell.org/cgi-bin/hackage-scripts/package/dlist">the dlist package</a> implementing it.  We don't require a whole fancy package just for this application, however; the changes are easy enough to make:

<pre>
import Prelude hiding ((+),(*),(**))
import qualified Prelude as P

type List a = [a] -&gt; [a]

fromList :: [a] -&gt; List a
fromList = (++)

toList :: List a -&gt; [a]
toList = ($[])

singleton :: a -&gt; List a
singleton = (:)

empty :: List a
empty = id

(+) = zipWith (.)
x * y = singleton (x,y)
x .* ys = map (x*) ys
ys *. x = map (*x) ys

(**) :: [a] -&gt; [b] -&gt; [List (a,b)]
[]     ** _      = []
_      ** []     = []
(x:xs) ** (y:ys) = x*y : (x .* ys) + (empty : (xs ** ys)) + (xs *. y)
</pre>

Ah, much better.  This code only takes 0.6 seconds on my machine to compute the same result with the first one million elements of <code>concat . map toList $ [1..] ** [1..]</code>.  

