---
katex: true
title: 'Anyone willing to help me get set up with something like miso?'
published: 2020-06-09T13:30:09Z
categories: haskell
tags: GHCJS,help,IDE,miso,web
---

<p>For the last few years I’ve been working (off and on) on a <a href="https://github.com/disco-lang/disco/">teaching language for FP and discrete math</a>. One of the big goals is to build a browser-based IDE which runs completely client-side, so students can code in their browsers without installing anything, and I don’t have to worry about running a server. Thanks to amazing technology like <a href="https://github.com/ghcjs/ghcjs">GHCJS</a>, <a href="https://haskell-miso.org/">miso</a>, <a href="https://reflex-frp.org/">reflex</a>, <em>etc.</em>, this seems like it should be entirely doable.</p>
<p>However, every time I sit down to try building such a thing, I end up getting completely bogged down in details of nix and stack and <code>.cabal</code> files and whatnot, and never even get off the ground. There are usually nice examples of building a new site from scratch, but I can never figure out the right way to incorporate my large amount of existing Haskell code. Should I have one package? Two separate packages for the website and the language implementation? How can I set things up to build either/both a command-line REPL and a web IDE?</p>
<p><strong>I’m wondering if there is someone experienced with GHCJS and miso who would be willing to help me get things set up.</strong> (I’m also open to being convinced that some other combination of technologies would be better for my use case.) I’m imagining a sort of pair-programming session via videoconference. I am pretty flexible in terms of times so don’t worry about whether your time zone matches mine. And if there’s something I can help you with I’m happy to do an exchange.</p>
<p>If you’re interested and willing, send me an email: <code>byorgey</code> at <code>gmail</code>. Thanks!</p>

