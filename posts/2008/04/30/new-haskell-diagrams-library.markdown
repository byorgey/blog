---
title: New Haskell diagrams library
published: 2008-04-30T17:21:35Z
categories: haskell,projects
tags: cairo,diagrams,DSL,EDSL,graphics,library
---

For the past week or so I've been working on an embedded domain-specific language for rendering simple diagrams with Haskell, and I'm excited to actually release version 0.1 today! You can now <a href="http://hackage.haskell.org/cgi-bin/hackage-scripts/package/diagrams-0.1">find it on Hackage</a>.  Version 0.1 is still fairly primitive, and there are a bunch more planned features, but you can already use it to create some pretty pictures.  Here are a few examples.

We'll start with a basic 'hello world' type diagram: a two-by-five rectangle, no frills:

<pre>
module Main where
import Graphics.Rendering.Diagrams

main = renderToPng "hello.png" 100 100 (rect 2 5)
</pre>


<a href='http://byorgey.files.wordpress.com/2008/04/hello.png'><img src="http://byorgey.files.wordpress.com/2008/04/hello.png?w=100" alt="" width="100" height="100" class="aligncenter size-medium wp-image-66" /></a>


OK, not too exciting, but at least it was easy.  Here's another silly example that shows off a few more available features:

<pre>
module Main where
import Graphics.Rendering.Diagrams

shapes :: Diagram
shapes = hcat [ fc blue $ circle 10
              , (fc goldenrod . lc green . lw 3 $ poly 5 10)
                ## (fc red . rotate (1/10) $ rect 4 4)
              , fc grey . lw 0 . scaleY 3 $ circle 5
              ]

main = renderToPng "shapes.png" 200 200 shapes
</pre>

<a href='http://byorgey.files.wordpress.com/2008/04/shapes.png'><img src="http://byorgey.files.wordpress.com/2008/04/shapes.png?w=200" alt="" width="200" height="200" class="aligncenter size-medium wp-image-67" /></a>

Hopefully, this example is fairly self-explanatory.  We can alter the appearance of diagrams by applying functions to them like fc (fill color), lc (line color), lw (line width), rotate, and scaleY.  We can superimpose two diagrams with ##.  And we can lay out a list of diagrams horizontally with hcat.  There are many other combinators along similar lines, with various options for distributing and aligning subdiagrams.

Now for a couple cooler examples.  How about a Sierpinski triangle?

<pre>
module Main where

import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Types

import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Diagrams.Shapes (draw)

data EqTri = EqTri  deriving Show
instance ShapeClass EqTri where
  shapeSize _   = (2, sqrt 3)
  renderShape _ = do
    c $ C.moveTo 1 s
    c $ C.lineTo 0 (-s)
    c $ C.lineTo (-1) s
    c $ C.closePath
    draw
   where s = sqrt 3 / 2

sierpinski :: Int -&gt; Diagram
sierpinski 0 = fc black $ lw 0 $
               shape EqTri
sierpinski n = vcatA hcenter [         s
                             ,      s &lt;&gt; s]
  where s = sierpinski (n-1)

main = renderToPng "sierpinski.png" 300 300 (sierpinski 6)
</pre>

<a href='http://byorgey.files.wordpress.com/2008/04/sierpinski.png'><img align="center" src="http://byorgey.files.wordpress.com/2008/04/sierpinski.png" alt="" width="300" height="300" class="aligncenter size-full wp-image-68" /></a>

This example illustrates a couple key points.  One is that the library is easy to extend with new shapes.  The built-in poly function is too general to provide a nice equilateral triangle for use in making a sierpinski triangle (its bounding box is too large, which would lead to ugly spaces in the diagram), so we can define our own shape just by making an instance of ShapeClass, and using the Cairo library to draw a path defining the shape.  This is probably not the best way to accomplish this particular task -- future versions of the diagrams library will include easier ways --  but it's a nice example of how easy it is to extend the basic library functionality.  

The other key point is how much power we get for free from the fact that this is an <em>embedded</em> DSL.  We can use the full power of Haskell to define a recursive function for computing sierpinski triangle diagrams.

For a final example, here are some nice Ford circles:

<pre>
module Main where

import Graphics.Rendering.Diagrams

import Data.Ratio
import System.Random

(&lt;+&gt;) :: Rational -&gt; Rational -&gt; Rational
r1 &lt;+&gt; r2 = (numerator r1 + numerator r2) % (denominator r1 + denominator r2)

farey :: Integer -&gt; [Rational]
farey 0 = [0%1, 1%1]
farey n = insertMediants (farey (n-1))

insertMediants :: [Rational] -&gt; [Rational]
insertMediants [] = []
insertMediants [x] = [x]
insertMediants (x:y:zs) = x : (x &lt;+&gt; y) : insertMediants (y:zs)

fordCircles :: Integer -&gt; [Diagram]
fordCircles n = map toCircle (filter ((&lt;= n) . denominator) $ farey n)

toCircle r = translateX r' $
             circle (1 / (2 * d'^2))
  where r' = fromRational r
        d' = fromIntegral (denominator r)

dia :: [Color] -&gt; Diagram
dia colors = view (0,-1/2) (1,0) $
             unionA hcenter bottom $
             zipWith fc colors (fordCircles 20)

randomColors :: [Double] -&gt; [Color]
randomColors (r:g:b:ds) = rgb r g b : randomColors ds

main :: IO ()
main = do
  g &lt;- newStdGen
  let rs = randoms g
  renderToPng "ford.png" 400 205 (dia $ randomColors rs)
</pre>

<a href='http://byorgey.files.wordpress.com/2008/04/ford.png'><img src="http://byorgey.files.wordpress.com/2008/04/ford.png" alt="" width="400" height="205" class="aligncenter size-full wp-image-70" /></a>

Plans for future versions of the library include:

<ul>
	<li>text objects</li>
	<li>settable backgrounds and better support for transparency</li>
	<li>support for line join style and dashing</li>
	<li>more primitive shapes: special triangles, ellipses, bezier curves, lines, arrows...</li>
	<li>more layouts: grid, tree, circle...</li>
	<li>constraint-based placement of objects, e.g. to connect diagrams with arrows</li>
	<li>more output modes: ps, svg, pdf</li>
	<li>and more!</li>
</ul>

If this looks interesting to you, I hope you'll <a href="http://hackage.haskell.org/cgi-bin/hackage-scripts/package/diagrams-0.1">download the library</a> and play around with it!  (Note that it does require the Cairo bindings, which are packaged as part of <a href="http://www.haskell.org/gtk2hs/">gtk2hs</a>, which is unfortunately not yet Cabalized.)  I would be happy to receive any and all feedback, including feature suggestions, bug reports, and pretty pictures.  If you're interested in contributing code, the darcs repository can be found at <a href="http://code.haskell.org/diagrams/">http://code.haskell.org/diagrams/</a>.

Enjoy!

