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
[`MonadRandom`](https://hackage.haskell.org/package/MonadRandom),
which defines a monad and monad transformer for generating random
values, along with a number of related utilities.

Recently, [Toni Dietze](https://github.com/Flupp) pointed out [a rare
situation that could cause the `fromListMay` function to
crash](https://github.com/byorgey/MonadRandom/issues/53).  This
function is supposed to draw a weighted random sample from a list of
values decorated with weights.  I'm not going to explain the details
of the issue here; suffice it to say that it has to do with
conversions between `Rational` (the type of the weights) and `Double`
(the type that was being used internally for generating random
numbers).

Even though this could only happen in rare and/or strange
circumstances, fixing it definitely seemed like the right thing to
do. After a bit of discussion, Flupp came up with a good suggestion
for a fix: we should no longer use `Double` internally for generating
random numbers, but rather `Word64`, which avoids conversion and
rounding issues.

In fact, `Word64` is already used in the generation of `Double`
values, so we can emulate that behavior (which was [a bit
tricky](https://github.com/byorgey/MonadRandom/issues/53#issuecomment-2294862625))
so that we make exactly the same random choices as before.

...or do we?  Well, no, because XXX

(`fromList`, `weighted`, `weightedMay`,
`uniform`, `uniformMay`)

To PVP or not to PVP?
---------------------

A function exported by the library has changed behavior, so
technically, [according to the Haskell PVP
specification](https://pvp.haskell.org/), this should be a major
version bump (*i.e.* `0.6` -> `0.7`). (Actually, I am not even 100% clear on this.  [The
decision tree](https://pvp.haskell.org/#decision-tree) on the PVP page
says that changing the *behavior* of an exported function necessitates
a major version bump; but the [actual
specification](https://pvp.haskell.org/#version-numbers) does not
refer to *behavior* at all---as I read it, it is exclusively concerned
with API compatibility, *i.e.* whether things will still compile.] But
there seem to be some good arguments for doing just a minor version
bump (*i.e.* `0.6` -> `0.6.1`).

* Arguments in favor of a minor version bump:

    - A major version bump would cause a lot of (unnecessary)
      breakage!  `MonadRandom` has [149 direct reverse dependencies,
      and about 3500 distinct transitive reverse
      dependencies](https://hackage.haskell.org/package/MonadRandom/reverse/verbose). Forcing
      all those packages to update their upper bound on `MonadRandom`
      would be a lot of churn.

    - What exactly constitutes the "behavior" of a function to
      generate random values?  It depends on the point of view.  If we
      view the function as just a pure mathematical function which
      happens to take a PRNG state as input and produces some value as
      output, then its behavior is defined precisely by which outputs
      it returns for which input seeds.  However, if we think of it in
      more effectful terms, we could say its "behavior" is just to
      output random values according to a certain distribution, in
      which case its behavior has *not* changed.

    - It's extremely unlikely that this change will cause any
      breakage; moreover, [as argued by Boyd Stephen Smith](https://mathstodon.xyz/@BoydStephenSmithJr@hachyderm.io/113250878388960212), anyone who cares enough about
      reproducibility to be relying on specific outputs for specific
      seeds is probably already pinning all their package versions.

* Arguments in favor of a major version bump:

    - It's what the PVP specifies; what's the point of having a
      specification if we don't follow it?

    - In the unlikely event that this change *does* cause any
      breakage, it could be extremely difficult for package
      maintainers to track down.  If the behavior of a random
      generation function completely changes, the source of the issue
      is obvious.  But if it only changes for very rare inputs, you
      might reasonably think the problem is something else.

So, do you have opinions on this?  Would the release affect you one
way or the other?  Note there has already been [a bit of discussion on
Mastodon](https://mathstodon.xyz/@byorgey/113250843195905599) as well.
