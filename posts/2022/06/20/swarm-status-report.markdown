---
katex: true
title: 'Swarm: status report'
published: 2022-06-20T16:26:36Z
categories: haskell,projects
tags: game,programming,resource,robot,Swarm
---

<a href="https://github.com/swarm-game/swarm/">Swarm</a> is a 2D programming and resource gathering game, written in Haskell. I <a href="https://byorgey.wordpress.com/2021/09/23/swarm-preview-and-call-for-collaboration/">announced it last September</a> and <a href="https://byorgey.wordpress.com/2021/10/01/swarm-a-lot-can-happen-in-a-week/">gave an update one week after that</a>, but haven’t written anything since then. However, that doesn’t mean development has stopped! Since last October, the repo has grown by an additional 4K lines of Haskell code (to 12K). Notable changes since then include:
<ul>
 	<li>Many UI improvements, like a main menu, ASCII art recipes, and mouse support</li>
 	<li>Many new entities and recipes (multiple types of motors, drills, and mines; new materials like iron, silver, quartz, and glass; new devices like circuits, clocks, cameras, comparators, compasses, …)</li>
 	<li>The game now supports <em>scenarios</em>, generically describing how to initialize the game and what the winning conditions should be: scenarios can be used to describe open-world games, tutorials, tricky programming challenges, … <em>etc.</em></li>
 	<li>Improvements to the programming language, like a new dedicated <code>robot</code> type, and a “delay” type for things that should be evaluated lazily</li>
 	<li>Increased emphasis on exploration, with the ability to <code>scan</code> unknown entities in the world</li>
</ul>
<div style="text-align:center;">

<img src="http://byorgey.files.wordpress.com/2022/06/main-menu.png" />

</div>
<div style="text-align:center;">

<img src="http://byorgey.files.wordpress.com/2022/06/trees.png" />

</div>
Development has picked up considerably in the past few weeks, and we’re making good progress toward a planned <a href="https://github.com/orgs/swarm-game/projects/1/views/5">alpha release</a> (though no concrete plans in terms of a release date yet). If you’re interested in getting involved, check out our <a href="https://github.com/swarm-game/swarm/blob/main/CONTRIBUTING.md">contribution guide</a>, come join us on IRC (<code>#swarm</code> on Libera.Chat) and take a look at the list of <a href="https://github.com/swarm-game/swarm/issues?q=is%3Aissue+is%3Aopen+label%3A%22C-Low+Hanging+Fruit%22">issues marked “low-hanging fruit”</a>—as of this writing there are 28 such issues, so plenty of tasks for everyone!

