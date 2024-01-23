---
title: The random graph
published: 2010-01-26T17:37:17Z
categories: grad school,humor
tags: graph theory,model theory,randomness
---

Today in my finite model theory class we learned about the <a href="http://en.wikipedia.org/wiki/Rado_graph">Rado graph</a>, which is a graph (unique up to isomorphism among countable graphs) with the extension property: given any two disjoint finite sets of vertices $latex U$ and $latex V$, there exists some other vertex $latex w$ which is adjacent to every vertex in $latex U$ and none of the vertices in $latex V$.  

This graph has some rather astonishing properties.  Here's one: consider starting with $latex n$ vertices and picking each edge with probability $latex 1/2$.  Clearly, there are $latex 2^{\binom n 2}$ different graphs you can get, each with equal probability; this defines a uniform random distribution over simple graphs with $latex n$ vertices.  What if you start with a countably infinite number of vertices instead?  The surprising answer is that with probability 1 you get the Rado graph.  Yes indeed, the Rado graph is <em>extremely</em> random.  It is so random that it is also called "THE random graph".

<pre>
SimpleGraph getRandomGraph()
{
    return radoGraph;  // chosen by fair coin flips.
                       // guaranteed to be random.
}
</pre>

(See <a href="http://xkcd.com/221/">http://xkcd.com/221/</a>.)

