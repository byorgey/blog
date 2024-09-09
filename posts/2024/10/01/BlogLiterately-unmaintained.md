---
title: 'Retiring BlogLiterately'
categories: haskell,meta
tags: blog,Hackage,Wordpress,open-source,BlogLiterately,HaXml,haxr
---

Way back in 2012 I took over maintainership of the [`BlogLiterately`
tool](https://hackage.haskell.org/package/BlogLiterately) from Robert
Greayer, its initial author.  I used it for many years to post to my
Wordpress blog, added a
[bunch](https://byorgey.github.io/blog/posts/2012/07/07/blogliterately-0-5-release.html)
of
[features](https://byorgey.github.io/blog/posts/2013/03/10/blogliterately-0-6-2.html),
solved some [fun
bugs](https://byorgey.github.io/blog/posts/2012/07/07/new-haxr-release.html),
and created the accompanying [`BlogLiterately-diagrams`
plugin](https://hackage.haskell.org/package/BlogLiterately-diagrams)
for embedding [diagrams](https://diagrams.github.io/) code in blog
posts.  However, now that I have fled Wordpress and rebuilt my blog
with [hakyll](https://jaspervdj.be/hakyll/), I don't use
`BlogLiterately` any more (there is even a `diagrams-pandoc` package
which does the same thing `BlogLiterately-diagrams` used to do).  So,
as of today I am officially declaring `BlogLiterately` unsupported.

The fact is, I haven't actually updated `BlogLiterately` since March
of last year. It currently only builds on GHC 9.4 or older, and no one
has complained, which I take as strong evidence that no one else is
using it either!  However, if anyone out there is actually using it,
and would like to take over as maintainer, I would be very happy to
pass it along to you.

I do plan to continue maintaining
[`HaXml`](https://hackage.haskell.org/package/HaXml) and
[`haxr`](https://hackage.haskell.org/package/haxr), at least for now;
unlike `BlogLiterately`, I know they are still in use, especially
`HaXml`.  However, `BlogLiterately` was really the only reason I cared
about these packages personally, so I would be happy to pass them
along as well; please get in touch if you would be willing to take
over maintaining one or both packages.
