---
title: 'Call for collaboration: Disco web UI'
categories: Haskell
katex: true
tags: disco,web,WASM,UI,teaching,discrete,math
---

**tl;dr**: I would like to have a web interface for [Disco](https://github.com/disco-lang/disco), a
student-oriented programming language for teaching functional
programming and discrete mathematics, which is implemented in Haskell.
I'm looking for others interested to help build it.  If you like
building web stuff with Haskell compiled to WASM and want to have a
positive impact on students learning mathematics and functional
programming, [get in touch](http://ozark.hendrix.edu/~yorgey/forest/contact/index.xml)!

## Disco

For the past nine (!) years I have been developing
[Disco](https://github.com/disco-lang/disco), a functional teaching
language for use in a discrete mathematics course.  It features
first-class functions, polymorphism, and recursive algebraic data
types, along with various built-in mathematical niceties and syntax
that is designed to be close to mathematical practice.

As a simple example, here's a recursive function in Disco to compute the
[hyperbinary numbers](https://oeis.org/A002487):

```
h : N -> N
h(0)      = 1
h(2n)     = h(n) + h(n .- 1)
h(2n + 1) = h(n)
```

Since recursive functions in Disco are automatically memoized, this
evaluates almost instantly even on very large numbers:

```
Disco> h(1000000)
1287
```

If you want to know more about Disco, you can [read a paper about it
here](http://ozark.hendrix.edu/~yorgey/forest/yorgey-disco-2023/index.xml),
or [read the language documentation](https://disco-lang.readthedocs.io/en/latest/).

## The problem

I want students in my [Discrete Mathematics
course](https://hendrix-cs.github.io/math240/) (or any Discrete
Mathematics course) to be able to use Disco: to play around at a REPL,
write and test their own code, and so on.

However, getting students to install Haskell and build Disco from
source is a total non-starter, for multiple reasons.  Some students in
the course are not all that tech-savvy.  Some students don't even have
their own computer, or the computer they do have chokes when trying to
compile a large Haskell project (*e.g.* because of limited memory).
Even aside from those issues, I simply don't want to spend time and
effort helping students install stuff at the beginning of the semester
(at least not in this class).

## Old solution: Replit

For a couple years, students were able to use Disco via a site called
Replit, which provided free educational accounts, and supported
arbitrary Nix configurations.  Since the [disco package on
Hackage](https://hackage.haskell.org/package/disco) was picked up by
nixpkgs, it was possible to spin up a Disco interpreter on Replit in
just a few seconds. Replit provided a virtual file system, an editor,
and, of course, a REPL.

This was a great solution while it lasted, but sadly it is no longer
viable:

- Disco has been broken in nixpkgs for a while now, and I have no idea
  how to fix it.
- Even if I could fix it, Replit has stopped supporting free education
  accounts, and started trying to shove LLMs into everything, making it no
  longer a viable teaching platform.

## Solution criteria

There are a few non-negotiable criteria that any solution must meet:

- It must either run on existing cloud infrastructure, or run
  completely client-side.  I do not want to be in the business of
  running a server, or of worrying about mitigating DOS attacks (by
  which I mean students accidentally running infinite loops generating
  infinite amounts of memory).  I also do not want to be in the
  business of managing accounts and logins.

- Students should not have to install anything.  As I mentioned
  before, this is a nonstarter for some students.

- It should allow students to work at a Disco REPL, and also test
  their own Disco code.

## Potential solutions

- Of course, a really nice solution would be to find an existing site
  which replicates some of the functionality we used to have with
  Replit and supports educational uses.  I kind of doubt such a thing
  exists, though I would be happy to be wrong about this.

- Another possibility is to have students use VSCode in their browsers
  via GitHub; to make this work we would have to add LSP support to
  Disco and package it up appropriately.  I'm not really excited about
  using GitHub for this, although implementing LSP for Disco is
  independently a great idea.

- The other solution I can think of is to compile Disco to WASM, and
  build a web UI on top of it, so that the whole thing runs in the
  student's browser.  I spent some time on this last year and
  successfully got Disco to compile to WASM, but didn't make it any
  farther than that.  Honestly, I simply don't know anything about web
  development, and I am not very motivated to learn.

## UI levels

Running with the last idea above (WASM + a custom web UI), what could
such a thing look like?  What features would we want?  Here are some
different solutions I could imagine, in increasing order of effort.

- **Level 0** would be to have just a web-based REPL.  Students can
  edit Disco code on their own device, upload it to the website, then
  evaluate expressions at the REPL.  Having to reupload their code
  every time they make changes would be annoying, but this would still
  be better than nothing.

- **Level 1** would be to have a web-based editor and REPL.  Students
  can type code in the editor, then push a button to load it into the
  REPL and play with it.

- **Level 2** would be to have a web-based filesystem + editor + REPL.
  Students can see a list of multiple files, edit them individually, and
  load any of them into the REPL.  The filesystem must live on their own
  computer, not on a remote server; but it could be their real
  filesystem, or a virtual filesystem.

I don't know how difficult any of these are; I would assume that
at least some of them can be achieved by gluing together some existing
Javascript components (e.g. [CodeMirror](https://codemirror.net/)?).
I'm sure it will require extending Disco itself with some new APIs,
but I can definitely work on that part once I know what would be needed.

If you know how to build web UIs like this and are interested in
helping, [get in touch](http://ozark.hendrix.edu/~yorgey/forest/contact/index.xml)!
