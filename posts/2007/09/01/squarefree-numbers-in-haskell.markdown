---
title: 'Squarefree numbers in Haskell'
published: 2007-09-02T02:26:32Z
categories: haskell,math
tags: 
---

A <em>squarefree</em> number is one which is not divisible by any perfect squares.  Put another way, the prime factorization of a squarefree number includes at most one copy of any given prime.  So, the first few squarefree numbers are

1,2,3,5,6,7,10,11,13,14,15,...

How can we generate these in Haskell?  In particular, we want to define the infinite sorted list of squarefree numbers, to be lazily computed on demand.  We could implement a brute-force method where we simply factor every positive integer and only keep those which don't have repeated factors.  But that would be awfully slow.  This is Haskell we're talking about, there must be a better way!

There are better ways, of course; here's one.  Suppose that (somehow) we have computed the list $L_n$, which contains all the squarefree numbers with prime factors up to and including $p_n$, the nth prime number.  For example, $L_1 = [1,2]$; $L_2 = [1,2,3,6]$; and so on.  Given $L_n$, we can compute $L_{n+1}$ as follows: multiply every element of $L_n$ by $p_{n+1}$ to produce a new list $L_{n+1}'$; this list contains all those squarefree numbers with prime factors up to $p_{n+1}$, which are divisible by $p_{n+1}$.  Then we just merge $L_n$ with $L_{n+1}'$ to produce $L_{n+1}$.

Let's try it: we start with $L_0 = [1]$, the list of all squarefree numbers with no prime factors.  Then we compute $L_1 = [1,2]$.  Multiplying by 3 gives $L_2' = [3,6]$, and merging yields $L_2 = [1,2,3,6]$.  Multiplying by 5 gives $L_3' = [5,10,15,30]$; merging again gives us $L_3 = [1,2,3,5,6,10,15,30]$.  And so on.

So, how do we translate this into a definition of the infinite list of squarefree numbers?  It's not quite as straightforward as it seems.  First of all, we can't just say the equivalent of "compute $L_{\infty}$"; nothing would ever get generated that way, since everything in $L_{n+1}$ depends on everything in $L_{n}$.  There's nothing inherent in the above method that indicates which part of the list isn't going to change on the next iteration.  And we can't do something like lazily merge all the lists in $L_1, L_2, L_3\dots$; the problem is that every squarefree number occurs infinitely often in such a list.

The key is to note that the "intermediate" lists $L_n'$ are more important than we might have thought.  The infinite sequence of lists $L_1', L_2', L_3',\dots$ in fact contains every squarefree number exactly once (except 1); moreover, they are ordered by their first elements in addition to being ordered themselves, which gives us just enough information to implement an infinite merge that will actually get around to producing something in a finite amount of time!

The <code>primeStep</code> function below takes the prime $p_{n+1}$ and the list $L_n$, and produces the pair $(L_{n+1}, L_{n+1}')$.  <code>mapAccumL</code> (one of those higher-order functions which isn't used much but comes in very handy every once in a while) is used to carry along the current list $L_n$ in an accumulator while simultaneously producing the list ${[L_1', L_2', L_3', \dots]} $.  Finally <code>mergeAll</code> performs a lazy infinite merge, giving us the infinite list of squarefree numbers.
<pre>
import Data.List (mapAccumL)

sieve (x:xs) = x : sieve (filter (n -&gt; n `mod` x /= 0) xs)
primes = sieve [2..]

-- merge two nondecreasing lists.
(#) :: (Ord a) =&gt; [a] -&gt; [a] -&gt; [a]
[] # ys = ys
xs # [] = xs
xs@(x:xt) # ys@(y:yt) | x &lt; y = x : (xt # ys)
                       | x &gt; y = y : (xs # yt)
                       | otherwise = x : (xt # yt)

-- merge an infinite list of lists, assuming that each list
-- is nondecreasing and the lists occur in order of their first
-- element.
mergeAll :: (Ord a) =&gt; [[a]] -&gt; [a]
mergeAll ([] : zs) = mergeAll zs
mergeAll (xxs@(x:xs) : yys@(y:ys) : zs)
    | x &lt; y = x : mergeAll (xs : yys : zs)
    | otherwise = mergeAll ((xxs # yys) : zs)

-- given a list of all squarefree numbers with factors up to
-- but not including p, produce (a,b), where a is all squarefree
-- numbers with factors up to p, and b only includes those which
-- are multiples of p.
primeStep xs p = (xs # pxs, pxs)
  where pxs = map (p*) xs

-- the nth element of primeLists is a sorted list of squarefree
-- numbers with factors up to p_n, which are all multiples of p_n.
-- Therefore every squarefree number occurs exactly once in (concat
-- primeLists), and the lists in primeLists are sorted by first element.
primeLists = snd $ mapAccumL primeStep [1] primes

-- to get a sorted list of squarefree numbers, just merge primeLists.
squarefree = 1 : mergeAll primeLists</pre>
It seems to work:
<pre>
*Main&gt; take 20 $ squarefree
[1,2,3,5,6,7,10,11,13,14,15,17,19,21,22,23,26,29,30,31]</pre>
Neat!

At the beginning, I said there <em>are</em> better ways; here's another!  It makes use of the beautiful theory of Dirichlet generating functions, and in particular, the fact that $\zeta(s)/\zeta(2s)$ is the DGF for the squarefree numbers.  Using David Amos' <a href="http://www.polyomino.f2s.com/david/haskell/main.html">fantastic mathematics library</a>, this is a piece of cake:
<pre>
import DirichletSeries
import Data.Maybe

bitToMaybe :: Int -&gt; a -&gt; Maybe a
bitToMaybe 1 = Just
bitToMaybe _ = const Nothing

squarefree = catMaybes $ zipWith bitToMaybe (coeffsDS squarefreeDGF) [1..]</pre>
Can you come up with another way?

(Thanks to chessguy, nasa_, olsner, and oerjan from #haskell for suggestions and ideas!)

