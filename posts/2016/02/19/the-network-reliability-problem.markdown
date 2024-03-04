---
katex: true
title: 'The network reliability problem'
published: 2016-02-19T19:30:00Z
categories: math
tags: graph,network,probability,reliability
---

<p>Let $G = (V,E)$ be a directed graph with vertices $V$ and edges $E$. Multiple edges between the same pair of vertices are allowed. For concreteness’ sake, think of the vertices as routers, and the edges as (one-way) connections. Let $\mathbb{P} = [0,1]$ denote the set of probabilities, and $\varphi : E \to \mathbb{P}$ be a function which assigns some probability to each edge. Think of $\varphi(e)$ as the probability that a single message sent along the edge $e$ from the source router will successfully reach the target router on the other end.</p>
<p>Suppose that when a router receives a message on an incoming connection, it immediately resends it on all outgoing connections. For $s,t \in V$, let $P(s,t)$ denote the probability that, under this “flooding” scenario, <em>at least one copy</em> of a message originating at $s$ will eventually reach $t$.</p>
<p>For example, consider the simple network shown below.</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2016/02/187e77faef9c21c1.png" alt="" /></p>
</div>
<p>A message sent from $s$ along the upper route through $r$ has an $0.5 \times 0.9 = 0.45$ probability of arriving at $t$. By definition a message sent along the bottom route has an $0.7$ probability of arriving at $t$. One way to think about computing the overall probability $P(s,t)$ is to compute the probability that it is <em>not</em> the case that the message <em>fails</em> to traverse <em>both</em> links, that is, $1 - (1 - 0.45)(1 - 0.7) = 1 - 0.165 = 0.835$. Alternatively, in general we can see that $1 - (1 - p)(1 - q) = p + q - pq$, so $0.45 + 0.7 - 0.45 \times 0.7 = 0.835$ as well. Intuitively, since the two events are not mutually exclusive, if we add them we are double-counting the situation where both links work, so we subtract the probability of both working.</p>
<p>The question is, given some graph $G$ and some specified nodes $s$ and $t$, how can we efficiently compute $P(s,t)$? For now I am calling this the “network reliability problem” (though I fully expect someone to point out that it already has a name). Note that it might make the problem a bit easier to restrict to directed <em>acyclic</em> graphs; but the problem is still well-defined even in the presence of cycles.</p>
<p>This problem turned out to be surprisingly more difficult and interesting than it first appeared. In a future post or two I will explain my solution, with a Haskell implementation. In the meantime, feel free to chime in with thoughts, questions, solutions, or pointers to the literature.</p>
<div id="refs" class="references">

</div>

