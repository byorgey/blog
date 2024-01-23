---
title: 'Collecting attributes'
published: 2009-10-28T18:59:03Z
categories: haskell,projects
tags: attributes,diagrams,monoid,overriding,priority
---

I'm proceeding with the <a href="http://byorgey.wordpress.com/2009/09/24/diagrams-0-2-1-and-future-plans/">redesign of my diagrams library</a> slowly but surely.  I don't have a lot of time to work on it, hence the "slowly".  But progress is being made.  It's still very much in the design phase, which makes it difficult for others to help, but <a href="http://www.haskell.org/haskellwiki/User:Lenny222">Lennart</a> has created a <a href="http://www.haskell.org/haskellwiki/Diagrams">diagrams page</a> on the Haskell wiki which I hope can be a good way to have an open design process and get input from the community.  There's a lot of stuff I have written down that I haven't gotten around to putting on that page yet, but I hope to do that soon.

Occasionally I plan to write some blog posts about interesting issues that arise in the design; today is the first.

Here's the background: a diagram is essentially a tree of sub-diagrams (although there will be a principled way to refer to subdiagrams in other parts of the tree; more on this in a future post), with leaves corresponding to atomic primitives.  Every node in the tree can (optionally) have some associated style attributes, such as stroke width and color, fill color, font, and other such things.  When we encounter a primitive at a leaf, how do we know what attributes to use when rendering it?  It may have some associated attributes, but its parent node might also have some associated attributes, and other ancestor nodes higher up the tree might have attributes as well.

What we'd really like to do is have a <code>Style</code> data type which has a <code>Monoid</code> instance:

<pre>
data Style = Style { strokeWidth :: Double
                   , strokeColour :: Colour
                   ... }

instance Monoid Style where
  ...
</pre>

Then to determine the style to use for a leaf, we just combine the styles of all its ancestors using the monoid.

So, how to implement this <code>Monoid</code> instance?  Well, first of all, what I wrote above is slightly bogus; at the very least each particular attribute ought to be optional, so it should look something more like this:

<pre>
data Style = Style { strokeWidth :: Maybe Double
                   , strokeColour :: Maybe Colour
                   ... }
</pre>

To combine two <code>Style</code> records, we match them up field-by-field, and <code>Just</code> trumps <code>Nothing</code>.  

So far, so good.  But how do we combine two fields containing <code>Just</code>?  It seems we have two choices: we can be biased towards the top of the tree (i.e. parent attributes override child attributes) or towards the bottom (i.e. child attributes override parent attributes).  Indeed, <code>Data.Monoid</code> contains two newtypes, <code>First</code> and <code>Last</code>, whose <code>Monoid</code> instances exhibit exactly this behavior.

But here's the problem: in this application, one of these choices isn't obviously better than the other.  In fact, it's easy to imagine situations where each would be the desired behavior.  For example, imagine that we have created a subdiagram that we want to use many times throughout a larger diagram.  Most of the time it will be blue, so it makes sense to specify that attribute as part of the subdiagram itself.  However, in one place we want it to be red, so we'd like to be able to override its attributes with parent attributes.  On the other hand, imagine a situation where a diagram is going to be composed of many different subdiagrams, which all share the property that they are blue.  To avoid repeating ourselves, it makes sense to specify "blueness" as an attribute of the parent diagram and have all the subdiagrams inherit it.  However, one subdiagram should be red, so we'd like to be able to override the parent attribute in this particular child.

What to do?  A first cut might look something like this:

<pre>
data Override a = Default | OverrideUp a | OverrideDown a

data Style = Style { strokeWidth :: Override Double
                   , strokeColour :: Override Colour
                   ... }
</pre>

The intention is that <code>OverrideUp a</code> overrides any attributes above/before it, and <code>OverrideDown a</code> overrides any attributes below/after it.  However, there's a problem: what should

<code>(OverrideDown a) `mappend` (OverrideUp b)</code>

be?  The <code>OverrideDown a</code> claims to override the <code>OverrideUp b</code>... and vice versa!  So this doesn't really work.  We need a way to specify relative priorities.  So, another solution would just be to assign each attribute with a priority:

<pre>
data Prioritized a = Default | Priority Double a

data Style = Style { strokeWidth :: Prioritized Double
                   , strokeColour :: Prioritized Colour
                   ... }
</pre>

For the <code>Monoid</code> instance, we just take the value with maximum priority.  This allows us to do what we wanted---overriding parent or child attributes is done simply by assigning a higher priority.  However, I <i>really</i> dislike this solution.  Having to specify a priority is annoying---but not only that, figuring out what priority to use to achieve your desired effect requires global knowledge about the value of the priorities used elsewhere.  One improvement we could make is to adopt the solution used by CSS: attributes are leaf-biased by default, but assigning a priority can override this.  That is,

<pre>
data Prioritized a = Default | Priority (Maybe Double) a
</pre>

where the <code>Monoid</code> instance chooses the value with the highest priority, or the right/leaf-most value if no priorities are specified.  This might be the best option---but it's still somewhat unsatisfactory.

I wonder about a solution that allows you to say, "I want to override the attribute on THAT node"---where "THAT" represents some way to refer to a particular node by name (what these names look like will be the subject of another post).  This might solve the problem of arbitrariness with the numerical priorities, but might also be veering into the realm of the overengineered...

