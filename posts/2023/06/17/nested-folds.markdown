---
title: Nested folds
published: 2023-06-17T21:54:56Z
categories: haskell
tags: ADH,fold,fusion,nested
---

<p>I’m finally getting around to reading <a href="https://www.cs.ox.ac.uk/publications/books/adwh/">Algorithm Design with Haskell</a> (hereafter abbreviated as ADH), by Jeremy Gibbons and Richard Bird. I’ve had it for a while, and I have no excuse for waiting this long to read it, but anyway. I’m enjoying it so far, and wanted to share something I (indirectly) learned. I’m sure there are some who already know this, but I didn’t. I’ll share both the fun takeaway and then also the interesting, roundabout path I took to get there.</p>
<h2 id="composed-folds-are-nested-folds">Composed folds are nested folds</h2>
<p>Here’s the punchline:</p>
<ul>
<li><code>foldl                 :: (b -&gt; a -&gt; b) -&gt; b -&gt;   [a]   -&gt; b</code></li>
<li><code>foldl . foldl         :: (b -&gt; a -&gt; b) -&gt; b -&gt;  [[a]]  -&gt; b</code></li>
<li><code>foldl . foldl . foldl :: (b -&gt; a -&gt; b) -&gt; b -&gt; [[[a]]] -&gt; b</code></li>
</ul>
<p>Actually, it’s a bit more general than this, since <code>foldl</code> works over any <code>Foldable</code>, not just lists: in fact, <code>foldl . foldl . foldl</code> can be used to fold any <code>t1 (t2 (t3 a))</code> as long as <code>t1</code>, <code>t2</code>, and <code>t3</code> are all instances of <code>Foldable</code>. For example, here is how we can add up all the integers contained in a <code>Maybe (Tree [Int])</code>:</p>
<pre><code>λ&gt; (foldl . foldl . foldl) (+) 0 (Just (Node [1,2,3] [Node [5,6] [], Node [7] [], Node [9,12] [Node [6] []]]))
51</code></pre>
<p>We can make sense of this if we look at the type of <code>foldl</code>. Its type is</p>
<pre><code>Foldable t =&gt; (b -&gt; a -&gt; b) -&gt; b -&gt; t a -&gt; b</code></pre>
<p>and we usually think of it as taking three arguments: a combining function of type <code>b -&gt; a -&gt; b</code>, an initial value of type <code>b</code>, and a structure to fold. But we can also think of it as a one-argument function. That is, it takes a function of type <code>b -&gt; a -&gt; b</code> and transforms it into a function of type <code>b -&gt; t a -&gt; b</code>:</p>
<pre><code>foldl :: Foldable t =&gt; (b -&gt; a -&gt; b) -&gt; (b -&gt; t a -&gt; b)</code></pre>
<p>Due to the magic of currying this is equivalent, and the second set of parentheses above is redundant. However, with this change of perspective it is easy to see what’s going on: the result of <code>foldl</code> is a function of type <code>b -&gt; t a -&gt; b</code>, which has the right shape to be the argument of <code>foldl</code> again, but this time with <code>t a</code> in place of <code>a</code>, yielding</p>
<pre><code>(b -&gt; t a -&gt; b) -&gt; (b -&gt; t2 (t a) -&gt; b)</code></pre>
<p>and so on.</p>
<p>What about <code>foldr</code>?</p>
<pre><code>foldr :: Foldable t =&gt; (a -&gt; b -&gt; b) -&gt; (b -&gt; t a -&gt; b)</code></pre>
<p>The shapes of the input and output to <code>foldr</code> don’t quite match, but they will if we throw in an extra <code>flip</code>, which switches the arguments of a two-argument function. So we can either do</p>
<pre><code>foldr . flip :: Foldable t =&gt; (b -&gt; a -&gt; b) -&gt; (b -&gt; t a -&gt; b)</code></pre>
<p>if we want to match the type of <code>foldl</code>, or</p>
<pre><code>flip . foldr :: Foldable t =&gt; (a -&gt; b -&gt; b) -&gt; (t a -&gt; b -&gt; b)</code></pre>
<p>In any case, we can now iterate just as with <code>foldl</code>; for example</p>
<pre><code>foldr . flip . foldr . flip . foldr :: (a -&gt; b -&gt; b) -&gt; (b -&gt; t1 (t2 (t3 a)) -&gt; b)</code></pre>
<p>(with some appropriate <code>Foldable</code> constraints thrown in as well).</p>
<h2 id="my-roundabout-journey">My roundabout journey</h2>
<p>So how did I come to realize this? Sometimes the journey is more interesting than the destination. The first chapter of ADH talks about the <em>foldr fusion rule</em>, which says that</p>
<pre><code>h . foldr f e == foldr g (h e)</code></pre>
<p>as long as <code>h (f x y) == g x (h y)</code> for all <code>x</code> and <code>y</code>. In other words, if we have a <code>foldr</code> followed by a function <code>h</code>, we can turn this into a single <code>foldr</code> (<em>i.e.</em> “fuse away” the <code>h</code>) as long as we can find an appropriate function <code>g</code> that satisfies the given criterion.</p>
<p>One of the exercises asks us to use <code>foldr</code> fusion to simplify</p>
<pre><code>foldr f e . concat</code></pre>
<p>which performs a fold over a nested list by first flattening the list and then doing a fold. You may wish to go try it yourself before reading on!</p>
<h2 id="the-solution">The solution</h2>
<p>We can compute as follows, where <code>g</code> is some function we will need to define appropriately:</p>
<pre><code>  foldr f e . concat
=                              { definition of concat }
  foldr f e . foldr (++) []
=                              { foldr fusion law, with h = foldr f e }
  foldr g (foldr f e [])
=                              { definition of foldr }
  foldr g e</code></pre>
<p>According to the fusion condition, we need <code>g</code> to satisfy <code>foldr f e (x ++ y) == g x (foldr f e y)</code>. This was already given as a previous exercise; I actually solved it by thinking about how <code>foldr</code> works and intuiting the right answer, but we can also calculate it using a second round of <code>foldr</code> fusion:</p>
<pre><code>  g x (foldr f e y)
=
  foldr f e (x ++ y)
=                              { definition of (++) }
  foldr f e (foldr (:) y x)
=                              { foldr fusion }
  foldr f (foldr f e y) x</code></pre>
<p>The last equation follows since <code>foldr f e (a : b) = f a (foldr f e b)</code> by definition of <code>foldr</code>. Hence, using <code>z</code> in place of <code>foldr f e y</code>, we have <code>g x z = foldr f z x = flip (foldr f) x z</code>, and so <code>g = flip (foldr   f)</code>, and we have</p>
<pre><code>foldr f e . concat = foldr (flip (foldr f)) e</code></pre>
<p>We can simplify even further: if we define <code>nestedFold f e = foldr f e . concat = foldr (flip (foldr f)) e</code> then we can eta-reduce to get <code>nestedFold = foldr . flip . foldr</code>.</p>
<p>When I derived this, my eyes bugged out and I started playing around with it, which is how I ended up figuring out the thing with <code>foldl</code>. Presumably, one could use the similar <code>foldl</code> fusion law to fuse <code>foldl f e . concat</code> and end up deriving the <code>foldl . foldl</code> result; I’ll leave that as an exercise for interested readers.</p>

