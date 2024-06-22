---
katex: true
title: 'Swarm: a lot can happen in a week'
published: 2021-10-01T19:31:58Z
categories: haskell,projects
tags: game,programming,resource,robot,Swarm
---

<p>It’s been about a week since I put out <a href="https://byorgey.github.io/blog/posts/2021/09/23/swarm-preview-and-call-for-collaboration.html">an announcement and call for collaboration</a> on a new game, <a href="https://github.com/swarm-game/swarm">Swarm</a>. Since then, the response has been fantastic: lots of people have tried it out, a few have even streamed themselves playing it on Twitch, and there has been lots of development activity.</p>
<ul>
<li>39 new <a href="https://github.com/swarm-game/swarm/issues">issues opened</a></li>
<li>27 <a href="https://github.com/swarm-game/swarm/pulls?q=is%3Apr+is%3Aclosed">pull requests merged</a></li>
<li>Lots of great discussion on GitHub issues as well as on IRC</li>
</ul>
<p>There’s still a long, long way to go before the game comes anywhere close to the vision for it, but we’ve made great progress! Some notable new features added since the initial announcement include:</p>
<ul>
<li><p>New <code>scan</code>, <code>upload</code>, and <code>install</code> commands</p></li>
<li><p>Semicolons are no longer required beetween consecutive <code>def</code>s</p></li>
<li><p>Basic help panel, and panel shortcut keys</p></li>
<li><p>Dramatically reduced CPU usage when idle</p></li>
<li><p>An overhaul of parsing and pretty-printing of constants (makes adding new constants easier, and an important prerequisite for saving definitions and games)</p></li>
<li><p>Better handling of water (you can make curry now)!</p>
<p><a href="https://github.com/swarm-game/swarm/blob/main/images/curry.png"><img src="http://byorgey.files.wordpress.com/2021/10/curry-1.png" /></a></p></li>
</ul>
<p>A couple more exciting things in progress that should land very soon:</p>
<ul>
<li><p>ASCII art recipes</p>
<p><a href="https://github.com/swarm-game/swarm/blob/main/images/recipes.png"><img src="http://byorgey.files.wordpress.com/2021/10/recipes.png" /></a></p></li>
<li><p>Basic editor integration via LSP, so you can write Swarm programs in your favorite editor with automatically highlighted syntax and type errors.</p></li>
</ul>
<p>And of course there are many other exciting things planned or in the works. <a href="https://github.com/swarm-game/swarm/">Come join us</a>!</p>

