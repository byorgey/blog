---
title: diagrams 0.2.1, and future plans
published: 2009-09-24T19:48:25Z
categories: haskell,projects
tags: constraint solving,declarative,diagrams,drawing,EDSL,rewrite
---

First of all, I've just released version 0.2.1 of the Haskell <a href="http://hackage.haskell.org/package/diagrams">diagrams library</a>.  This is a minor release which fixes a few bugs and adds a few new combinators, most notably a grid layout combinator contributed by Ganesh Sittampalam.  For more information and a full list of the features new to 0.2.1, see the <a href="http://code.haskell.org/diagrams/">diagrams web page</a>.

The real reason for the release, however, is to get existing new features out the door before gearing up for a planned major rewrite of the backend to use a constraint-solving layout engine.  This will allow for much greater elegance and flexibility, as well as a number of features (such as arrows connecting different parts of the diagram) which would be difficult or impossible to implement in the current framework.

My ultimate vision is for the diagrams library to become a viable alternative to declarative drawing systems such as <a href="http://www.tug.org/metapost.html">MetaPost</a> and <a href="http://asymptote.sourceforge.net/">Asymptote</a>, with the distinct advantages that it will be
<ul>
	<li><i>purely</i> declarative, and</li>
	<li>an <i>embedded</i> DSL, providing the full power of Haskell and its ecosystem, as opposed to the ad-hoc specialized languages used by MetaPost and Asymptote.</li>
</ul>

If this sounds exciting to you, I hope you'll join me, either by trying out diagrams for your projects and providing feedback, or by contributing some code.  If you're interested in helping with the rewrite itself, let me know; I also plan to set up a core/contrib model like that of <a href="http://xmonad.org">xmonad</a>, so there should also be plenty of opportunities for contributing independent add-on modules which enhance the core functionality.

