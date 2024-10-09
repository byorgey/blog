---
title: 'MonadRandom: major or minor version bump?'
categories: haskell,meta
tags: Hackage,MonadRandom,random,version
---

**tl;dr**: a fix to the `MonadRandom` package may cause `fromListMay`
and related functions to *very rarely* output different results than
they used to.  This could only possibly affect anyone who is using
fixed seed(s) to generate random values and is depending on the
specific values being produced, *e.g.* a unit test where you use a
specific seed and test that you get a specific result.  Do you think
this should be a major or minor version bump?

<hr />

The Fix
-------

Since 2013 I have been the maintainer of
[`MonadRandom`](https://hackage.haskell.org/package/MonadRandom), a
package which defines XXX.

Recently, [Toni Dietze](https://github.com/Flupp) pointed out [a rare situation that could cause the
`fromListMay` function to
crash](https://github.com/byorgey/MonadRandom/issues/53).  This
function is supposed to draw a weighted random sample from a list of
values with weights.  The purpose of this blog post is not to explain
the details of the issue; suffice it to say that it has to do with
conversions between `Rational` (the type of the weights) and `Double`
(the type used internally for generating random numbers).

Even though this could only happen in rare and/or strange
circumstances, fixing it seemed like the right thing to do. After a
bit of discussion, Flupp came up with a good suggestion for a fix: we
should no longer use `Double` internally for generating random
numbers, but rather `Word64`, which avoids conversion issues.

In fact, `Word64` is already used in the generation of `Double`
values, so we can emulate that behavior (which was [a bit
tricky](https://github.com/byorgey/MonadRandom/issues/53#issuecomment-2294862625))
so that we make exactly the same random choices as before.

...or do we?  Well, no, because XXX

To PVP or not to PVP?
---------------------

So here is the situation:

* Arguments for minor version bump:

    - A major version bump would cause a lot of (unnecessary)
      breakage!  `MonadRandom` has 149 direct reverse dependencies,
      and 8143 transitive reverse dependencies.

    - XXX argued that if you care enough about reproducibility to be
      relying on specific outputs for specific seeds, then you are
      probably pinning all your package versions anyway.

* Arguments for major version bump:

    - Breakages could be very difficult to track down.  If it
      completely changed it would be obvious.  If it only changes very
      rarely you might think it was something else.


(`fromList`, `weighted`, `weightedMay`,
`uniform`, `uniformMay`)
