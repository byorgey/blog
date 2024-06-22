---
katex: true
title: 'a chicken monad'
published: 2009-02-06T19:39:24Z
categories: humor,meta
tags: chicken,monads
---

Yesterday, <a href="http://blog.plover.com/">Mark Dominus</a> took a picture of me eating <a href="https://byorgey.github.io/blog/posts/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy.html">a monad</a>:

<img src="http://byorgey.files.wordpress.com/2009/02/15794109057.jpg?w=225" alt="Brent eating a monad" title="Brent eating a monad" width="225" height="300" class="aligncenter size-medium wp-image-130" />

It was a delicious <code>Chicken</code> monad.

<pre>
data Chicken a = Chicken (Egg a)
data Egg a = Egg (Chicken a)

instance Monad Chicken where
  ...(exercise for the reader)
</pre>

