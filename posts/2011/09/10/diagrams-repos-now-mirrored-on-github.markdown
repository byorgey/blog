---
katex: true
title: 'diagrams repos now mirrored on github!'
published: 2011-09-11T00:26:45Z
categories: haskell,projects
tags: bridge,darcs,diagrams,git
---

I was quite excited when I learned that <a href="http://www.owenstephens.co.uk/" title="Owen Stephens">Owen Stephens</a> was working on a two-way darcs-git bridge for his Google Summer of Code project.  You see, I'm one of those holdouts who, despite acknowledging that <a href="http://github.com" title="GitHub">github</a> is pretty cool and a lot of people in the Haskell community are using it these days, still really loves <a href="http://darcs.net" title="darcs">darcs</a> and refuses to give it up.  Well, thanks to <a href="http://blog.owenstephens.co.uk/201109/gsoc-darcs-bridge-%E2%80%93-results" title="GSoC: Darcs Bridge - Results">Owen's great work</a>, it looks like I can now have my cake and eat it too.

I've now mirrored four <a href="http://projects.haskell.org/diagrams" title="diagrams">diagrams</a> repositories on github:

<ul>
	<li><a href="https://github.com/byorgey/diagrams-core" title="diagrams-core">diagrams-core</a> - core library</li>
	<li><a href="https://github.com/byorgey/diagrams-lib" title="diagrams-lib">diagrams-lib</a> - standard library of primitives and combinators built on top of diagrams-core</li>
	<li><a href="https://github.com/byorgey/diagrams-cairo" title="diagrams-cairo">diagrams-cairo</a> - rendering backend using cairo</li>
        <li><a href="https://github.com/byorgey/diagrams-doc" title="diagrams-doc">diagrams-doc</a> - documentation for the project, including source code for the <a href="http://projects.haskell.org/diagrams" title="Diagrams">website</a>, tutorial, user manual, IRC logs, etc.</li>
</ul>

I will continue to use darcs as my primary VCS, and of course I will still accept darcs patches (the darcs repos are <a href="http://patch-tag.com/r/byorgey/">hosted on patch-tag.com</a>).  But I can now accept pull requests on github, for contributors who prefer using git/github.  It will probably take a while to get the kinks worked out and figure out how to manage everything properly.  But I'm excited by the opportunity to encourage more collaboration.

Oh yes, and the user manual is coming along nicely!  You can see a <a href="http://www.cis.upenn.edu/~byorgey/diagrams-manual/diagrams-manual.html" title="Diagrams user manual (preview)">preview version here</a>.  Comments and patches (via either darcs or git!) welcome.  There have also been quite a few cool features added since the 0.3 release, and we hope to have an 0.4 release out soon.

