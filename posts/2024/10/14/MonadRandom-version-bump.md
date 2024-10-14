---
title: 'MonadRandom: major or minor version bump?'
categories: haskell,meta
tags: Hackage,MonadRandom,random,version,PVP
katex: true
---

**tl;dr**: a fix to the `MonadRandom` package may cause `fromListMay`
and related functions to *extremely rarely* output different results than
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
crash](https://github.com/byorgey/MonadRandom/issues/53) (as well as
the other functions which depend on it: `fromList`, `weighted`,
`weightedMay`, `uniform`, and `uniformMay`).  This function is
supposed to draw a weighted random sample from a list of values
decorated with weights.  I'm not going to explain the details of the
issue here; suffice it to say that it has to do with conversions
between `Rational` (the type of the weights) and `Double` (the type
that was being used internally for generating random numbers).

Even though this could only happen in rare and/or strange
circumstances, fixing it definitely seemed like the right thing to
do. After a bit of discussion, Toni came up with a good suggestion
for a fix: we should no longer use `Double` internally for generating
random numbers, but rather `Word64`, which avoids conversion and
rounding issues.

In fact, `Word64` is already used internally in the generation of
random `Double` values, so we can emulate the behavior of the `Double`
instance (which was [slightly
tricky](https://github.com/byorgey/MonadRandom/issues/53#issuecomment-2294862625)
to figure out)
so that we make exactly the same random choices as before, but without
actually converting to `Double`.

The Change
----------

...well, not *exactly* the same random choices as before, and therein
lies the rub!  If `fromListMay` happens to pick a random value which
is extremely close to a boundary between choices, it's possible that
the value will fall on one side of the boundary when using exact
calculations with `Word64` and `Rational`, whereas before it would
have fallen on the other side of the boundary after converting to
`Double` due to rounding. In other words, it will output the
same results *almost all the time*, but for a list of $n$ weighted
choices there is something like an $n/2^{64}$ chance (or less) that
any given random choice will be different from what it used to be.  I
have never observed this happening in my tests, and indeed, I do not
expect to ever observe it!  If we generated one billion random samples
per second continuously for a thousand years, we might expect to see
it happen once or twice.  I am not even sure how to engineer a test
scenario to force it to happen, because we would have to pick an
initial PRNG seed that forces a certain `Word64` value to be
generated.

To PVP or not to PVP?
---------------------

Technically, a function exported by `MonadRandom` has changed
behavior, so [according to the Haskell PVP
specification](https://pvp.haskell.org/) this should be a major
version bump (*i.e.* `0.6` to `0.7`).^[Actually, I am not even
100% clear on this.  [The decision
tree](https://pvp.haskell.org/#decision-tree) on the PVP page says
that changing the *behavior* of an exported function necessitates a
major version bump; but the [actual
specification](https://pvp.haskell.org/#version-numbers) does not
refer to *behavior* at all---as I read it, it is exclusively concerned
with API compatibility, *i.e.* whether things will still compile.] But
there seem to be some good arguments for doing just a minor version
bump (*i.e.* `0.6` to `0.6.1`).

* Arguments in favor of a minor version bump:

    - A major version bump would cause a lot of (probably unnecessary)
      breakage!  `MonadRandom` has [149 direct reverse dependencies,
      and about 3500 distinct transitive reverse
      dependencies](https://hackage.haskell.org/package/MonadRandom/reverse/verbose). Forcing
      all those packages to update their upper bound on `MonadRandom`
      would be a lot of churn.

    - What exactly constitutes the "behavior" of a function to
      generate random values?  It depends on your point of view.  If
      we view the function as a pure mathematical function which
      takes a PRNG state as input and produces some value as
      output, then its behavior is defined precisely by which outputs
      it returns for which input seeds, and its behavior has changed.
      However, if we think of it in more effectful terms, we could say
      its "behavior" is just to output random values according to a
      certain distribution, in which case its behavior has *not*
      changed.

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
      might reasonably think the problem is something else.  A major
      version bump will force maintainers to read the changelog for
      `MonadRandom` and assess whether this is a change that could
      possibly affect them.

So, do you have opinions on this?  Would the release affect you one
way or the other?  Feel free to leave a comment here, or send me an
email with your thoughts. Note there has already been [a bit of discussion on
Mastodon](https://mathstodon.xyz/@byorgey/113250843195905599) as well.
