---
title: Diagrams 0.2 release
published: 2009-01-31T23:35:37Z
categories: haskell,projects
tags: announcement,diagrams,Heighway dragon,library,release
---

After meaning to get around to it for quite a while, I've finally released version 0.2 of the Haskell <a href="http://code.haskell.org/diagrams/">diagrams library</a>.  Here's the <a href="http://www.haskell.org//pipermail/haskell-cafe/2009-January/054669.html">release announcement</a>. And here's one of my favorite examples showing off the new path support:

[caption id="attachment_120" align="aligncenter" width="300" caption="Heighway dragon"]<img src="http://byorgey.files.wordpress.com/2009/01/dragon.png" alt="Heighway dragon" title="dragon" width="300" height="453" class="size-full wp-image-120" />[/caption]

I made this <a href="http://en.wikipedia.org/wiki/Dragon_curve">Heighway dragon curve</a> in just a few minutes of hacking this afternoon, with the following code:

<pre>
{- Heighway dragon.  See http://en.wikipedia.org/wiki/Dragon_curve. -}
module Main where

import Graphics.Rendering.Diagrams
import Control.Monad.State
import Data.Maybe

dragonStr :: Int -&gt; String
dragonStr 0 = "FX"
dragonStr n = concatMap rules $ dragonStr (n-1)
  where rules 'X' = "X+YF+"
        rules 'Y' = "-FX-Y"
        rules c = [c]

strToPath :: String -&gt; Path
strToPath s = pathFromVectors . catMaybes $ evalState c (0,-1)
  where c        = mapM exec s
        exec 'F' = Just `fmap` get
        exec '-' = modify left &gt;&gt; return Nothing
        exec '+' = modify right &gt;&gt; return Nothing
        exec _   = return Nothing
        left (x,y)  = (-y,x)
        right (x,y) = (y,-x)

dragon :: Int -&gt; Diagram
dragon = lc red . curved 0.8 . strToPath . dragonStr

main = renderAs PNG "dragon.png" (Width 300) (dragon 12)
</pre>

A special thank you to <a href="http://www.dougalstanton.net/blog/">Dougal Stanton</a> for adding text rendering support and other features, switching diagrams over to <a href="http://r6.ca/">Russell O'Connor's</a> <a href="http://hackage.haskell.org/cgi-bin/hackage-scripts/package/colour">colour library</a>, and generally helping out with this release.

