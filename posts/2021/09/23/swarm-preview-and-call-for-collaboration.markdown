---
title: Swarm: preview and call for collaboration
published: 2021-09-24T02:51:02Z
categories: haskell,projects
tags: game,programming,resource,robot,Swarm
---

<p>For about a month now I have been working on building a game<a href="#fn1" class="footnote-ref" id="fnref1" role="doc-noteref"><sup>1</sup></a>, tentatively titled <a href="https://github.com/swarm-game/swarm">Swarm</a>. It’s nowhere near finished, but it has at least reached a point where I’m not embarrassed to show it off. I would love to hear feedback, and I would especially love to have others contribute! Read on for more details.</p>
<p><a href="https://github.com/swarm-game/swarm"><img src="http://byorgey.files.wordpress.com/2021/09/log.png" /></a></p>
<p>Swarm is a 2D tile-based resource gathering game, but with a twist: the only way you can interact with the world is by building and programming robots. And there’s another twist: the kinds of commands your robots can execute, and the kinds of programming language features they can interpret, depends on what devices they have installed; and you can create new devices only by gathering resources. So you start out with only very basic capabilities and have to bootstrap your way into more sophisticated forms of exploration and resource collection.</p>
<p>I guess you could say it’s kind of like a cross between Minecraft, Factorio, and <a href="https://en.wikipedia.org/wiki/Karel_(programming_language)">Karel the Robot</a>, but with a much cooler programming language (lambda calculus + polymorphism + recursion + exceptions + a command monad for first-class imperative programs + a bunch of other stuff).</p>
<p>The game is far from complete, and especially needs a lot more depth in terms of the kinds of devices and levels of abstraction you can build. But for me at least, it has already crossed the line into something that is actually somewhat fun to play.</p>
<p>If it sounds interesting to you, give it a spin! Take a look at the <a href="https://github.com/swarm-game/swarm/blob/main/README.md">README</a> and the <a href="https://github.com/swarm-game/swarm/blob/main/TUTORIAL.md">tutorial</a>. If you’re interested in contributing to development, check out the <a href="https://github.com/swarm-game/swarm/blob/main/CONTRIBUTING.md">CONTRIBUTING</a> file and the <a href="https://github.com/swarm-game/swarm/issues">GitHub issue tracker</a>, which I have populated with a plethora of tasks of varying difficulty. This could be a great project to contribute to especially if you’re relatively new to Haskell; I try to keep everything well-organized and well-commented, and am happy to help guide new contributors.</p>
<section class="footnotes" role="doc-endnotes">
<hr />
<ol>
<li id="fn1" role="doc-endnote"><p>Can you tell I am on sabbatical?<a href="#fnref1" class="footnote-back" role="doc-backlink">↩︎</a></p></li>
</ol>
</section>

