---
title: 'Monads are not like burritos'
categories: Haskell
tags: monad,pedagogy,meme,burrito,analogy
---

In January 2009, while just a baby first-year PhD student, I wrote a
blog post titled [Abstraction, intuition, and the "monad tutorial
fallacy"](https://byorgey.github.io/blog/posts/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy.html).
In it, I made the argument that humans tend to learn best by first
grappling with concrete examples, and only later proceeding to
higher-level intuition and analogies later; hence, it's a mistake to
think that clearly presenting your intuition for a topic will help
other people understand it.  Analogies and intuition can help, but
only when accompanied by concrete examples and active engagement.  To
illustrate the point, I made up a fictitious programmer with a
fictitious analogy.

> But now Joe goes and writes a monad tutorial called “Monads are
> Burritos,” under the well-intentioned but mistaken assumption that
> if other people read his magical insight, learning about monads will
> be a snap for them. “Monads are easy,” Joe writes. “Think of them as
> burritos.” Joe hides all the actual details about types and such
> because those are scary, and people will learn better if they can
> avoid all that difficult and confusing stuff.  Of course, exactly
> the opposite is true, and all Joe has done is make it _harder_ for
> people to learn about monads...

My intention was to choose a fictitious analogy which was obviously
ridiculous and silly, as a parody of many of the monad tutorials which
existed at the time (and still do).  [Mark Jason Dominus](https://blog.plover.com/meta/about-me.html)
then wrote a blog post, [Monads are like
burritos](https://blog.plover.com/prog/burritos.html), pointing out
that actually, monads _are_ kinda like burritos.  It's really funny,
though I don't think it's actually a very good analogy, and my guess
is that Mark would agree: it was clearly written as a silly joke and
not as a real way to explain monads.

In any case, from that point the "monads are burritos" meme took on a
life of its own.  For example:

- [Chris Done made a webcomic about
  it](https://chrisdone.com/posts/monads-are-burritos/)
- [Ed Morehouse wrote a ridiculous paper exploring the categorical
  foundations of burritos](https://edwardmorehouse.github.io/silliness/burrito_monads.pdf)
- [Someone made a `burrito` library in Rust](https://github.com/withoutboats/burrito)
- [Dr Eugenia Cheng tweeted about it](https://x.com/DrEugeniaCheng/status/1316817271961116679)

I even joined in the fun and made this meme image about bad monad
tutorials:

![](http://ozark.hendrix.edu/~yorgey/pub/monad_tutorial.jpg)

Of course there are [lots of people who still understand that it was all just a silly joke](https://www.reddit.com/r/haskell/comments/6bxk1v/why_monads_always_get_compared_to_burritos/).
Recently, however, I've seen several instances where people apparently
believe "monads are burritos" is a real, helpful thing and not just a
joke meme.  For example, see [this thread on
lobste.rs](https://lobste.rs/s/xmpj1p/you_probably_wrote_half_monad_by_accident),
or [this Mastodon post](https://mathstodon.xyz/@CubeRootOfTrue/114404282908533701).

So, to set the record straight: "monads are burritos" is _not_ a helpful
analogy!^[Yes, I am writing a blog post because [People Are Wrong On
The Internet](https://xkcd.com/386/), and I know it probably won't
make any difference, but here we are.]  Why not, you ask?
To expand on my reasons from a [10-year-old Reddit
comment](https://www.reddit.com/r/haskell/comments/3bdrlj/comment/ct24jmc/):

- The burrito analogy strongly implies that a value of type `m a`
  somehow "contains" a value (or values) of type `a`.  But that is not
  true for all monads (e.g. there is no sense in which a value of type
  `IO String` contains a `String`).
- Relatedly, the analogy also implies that a value of type `m a` can
  be "unwrapped" to get an `a`, but this is impossible for many monads.
- It is not actually very easy to take a burrito containing a burrito
  and merge it into a single-level burrito.  At least this is not in
  any sense a natural operation on burritos.  Perhaps you could argue
  that it is always easy to remove outer tortilla layers (but not the
  innermost one since the food will all fall out), but this is a bad
  analogy, since in general `join` does not just "remove" an outer
  layer, but somehow merges the effects of two layers into one.

Actually, burritos are a great analogy for the `Identity` monad!
...but not much beyond that.

On a more positive note, my sense is that the average
pedagogical quality of Haskell materials, and monad tutorials in
particular, has indeed gone up significantly since 2009.  I'd love to
think this can be at least partially attributed to my original blog
post, though of course it's impossible to know that for sure.
