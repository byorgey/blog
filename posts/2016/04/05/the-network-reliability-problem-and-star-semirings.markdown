---
title: 'The network reliability problem and star semirings'
published: 2016-04-06T01:28:24Z
categories: math
tags: graph,network,probability,reliability,semiring,star
---

<p>In a <a href="https://byorgey.wordpress.com/2016/02/19/the-network-reliability-problem/">previous post</a> I defined the <em>network reliability</em> problem. Briefly, we are given a directed graph whose edges are labelled with probabilities, which we can think of as giving the likelihood of a message successfully traversing a link in a network. The problem is then to compute the probability that a message will successfully traverse the network from a given source node to a given target node.</p>
<p>Several commenters pointed out the connection to <a href="https://en.wikipedia.org/wiki/Bayesian_network">Bayesian networks</a>. I think they are right, and the network reliability problem is a very special case of Bayesian inference. However, so far this hasn’t seemed to help very much, since the things I can find about algorithms for Bayesian inference are either too general (<em>e.g.</em> allowing arbitrary functions at nodes) or too specific (<em>e.g.</em> only working for certain kinds of trees). So I’m going to put aside Bayesian inference for now; perhaps later I can come back to it.</p>
<p>In any case, <a href="https://byorgey.wordpress.com/2016/02/19/the-network-reliability-problem/#comment-15064">Derek Elkins also made a comment</a> which pointed to exactly what I wanted to talk about next.</p>
<h1 id="star-semirings-and-path-independence">Star semirings and path independence</h1>
<p>Consider the related problem of computing the reliability of the <em>single most reliable path</em> from $s$ to $t$ in a network. This is really just a disguised version of the shortest path problem, so one can solve it using <a href="https://en.wikipedia.org/wiki/Dijkstra&#039;s_algorithm">Dijkstra’s algorithm</a>. But I want to discuss a more general way to think about solving it, using the theory of star semirings. Recall that a <em>semiring</em> is a set with two associative binary operations, “addition” and “multiplication”, which is a commutative monoid under addition, a monoid under multiplication, and where multiplication distributes over addition and $0a = a0 = 0$. A <em>star semiring</em> is a semiring with an additional operation $(-)^*$ satisfying $a^* = 1 + aa^* = 1 + a^*a$. Intuitively, $a^* = 1 + a + a^2 + a^3 + \dots$ (though $a^*$ can still be well-defined even when this infinite sum is not; we can at least say that <em>if</em> the infinite sum is defined, they must be equal). If $S$ is a star semiring, then the semiring of $n \times n$ matrices over $S$ is also a star semiring; for details see <span class="citation">Dolan (2013)</span>, <span class="citation">O’Connor (2011)</span>, <span class="citation">Penaloza (2005)</span>, and <span class="citation">Lehmann (1977)</span>. In particular, there is a very nice functional algorithm for computing $M^*$, with time complexity $O(n^3)$ <span class="citation">(Dolan 2013)</span>. (Of course, this is slower than Dijkstra’s algorithm, but unlike Dijkstra’s algorithm it also works for finding shortest paths in the presence of negative edge weights—in which case it is essentially the <a href="https://en.wikipedia.org/wiki/Floyd-Warshall_algorithm">Floyd-Warshall algorithm</a>.)</p>
<p>Now, given a graph $G = (V,E)$ and labelling $\varphi : E \to \mathbb{P}$, define the $|V| \times |V|$ adjacency matrix $M_G$ to be the matrix of edge probabilities, that is, $(M_G)_{uv} = \varphi(u,v)$. Let $(\mathbb{P}, \max, 0, \times, 1)$ be the star semiring of probabilities under maximum and multiplication (where $a^* = 1$, since $1 = \max(1,a \times 1)$). Then we can solve the single most reliable path problem by computing $M_G^*$ over this semiring, and finding the largest entry. If we want to find the actual most reliable path, and not just its reliability, we can instead work over the semiring $\mathbb{P} \times E^*$, <em>i.e.</em> probabilities paired with paths. You might enjoy working out what the addition, multiplication, and star operations should be, or see <span class="citation">O’Connor (2011)</span>.</p>
<p>In fact, as shown by O’Connor and Dolan, there are many algorithms that can be recast as computing the star of a matrix, for an appropriate choice of semiring: for example, (reflexive-)transitive closure; all-pairs shortest paths; Gaussian elimination; dataflow analysis; and solving certain knapsack problems. One might hope that there is similarly an appropriate semiring for the network reliability problem. But I have spent some time thinking about this and I do not know of one.</p>
<p>Consider again the simple example given at the start of the <a href="https://byorgey.wordpress.com/2016/02/19/the-network-reliability-problem/">previous post</a>:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2016/04/0c0251dc858b7992.png" /></p>
</div>
<p>For this example, we computed the reliability of the network to be $0.835$, by computing the probability of the upper path, $p = 0.45$, and the lower path, $q = 0.7$, and then combining them as $p + q - pq$, the probability of success on either path less the double-counted probability of simultaneous success on both.</p>
<p>Inspired by this example, one thing we might try would be to define operations $p \land q = pq$ and $p \lor q = p + q - pq$. But when we go to check the semiring laws, we run into a problem: distributivity does not hold! $p \land (q \lor r) = p(q + r - qr) = pq + pr - pqr$, but $(p \land q) \lor (p \land r) = pq \lor pr = pq + pr - p^2qr$. The problem is that the addition operation $p \lor q = p + q - pq$ implicitly assumes that the events with probabilities $p$ and $q$ are <em>independent</em>: otherwise the probability that they both happen is not actually equal to $pq$. The events with probabilities $pq$ and $pr$, however, are <em>not</em> independent. In graph terms, they represent two paths with a shared subpath. In fact, our example computation at the beginning of the post was only correct since the two paths from $s$ to $t$ were completely independent.</p>
<h1 id="graph-reduction">Graph reduction</h1>
<p>We can at least compute the reliability of <a href="https://en.wikipedia.org/wiki/Series-parallel_graph">series-parallel graphs</a> whose terminals correspond with $s$ and $t$:</p>
<ul>
<li>If $G$ consists of a single edge, return that edge’s probability.</li>
<li>Otherwise, $G$ is a composition of two subgraphs, whose reliabilities we recursively compute. Then:
<ul>
<li>If $G$ is a sequential composition of graphs, return the product of their reliabilities.</li>
<li>If $G$ is a parallel composition of two graphs with reliabilities $p$ and $q$, return $p + q - pq$.</li>
</ul></li>
</ul>
<p>In the second case, having a parallel composition of graphs ensures that there are no shared edges between them, so $p$ and $q$ are indeed independent.</p>
<p>Of course, many interesting graphs are not series-parallel. The simplest graph for which the above does not work looks like this:</p>
<div style="text-align:center;">
<p><img src="http://byorgey.files.wordpress.com/2016/04/25b7575a4c860ccd.png" /></p>
</div>
<p>Suppose all the edges have probability $1/3$. Can you find the reliability of this network?</p>
<p>More in a future post!</p>
<h1 id="references" class="unnumbered">References</h1>
<div id="refs" class="references">
<div id="ref-dolan2013fun">
<p>Dolan, Stephen. 2013. “Fun with Semirings: A Functional Pearl on the Abuse of Linear Algebra.” In <em>ACM SIGPLAN Notices</em>, 48:101–10. 9. ACM.</p>
</div>
<div id="ref-lehmann1977algebraic">
<p>Lehmann, Daniel J. 1977. “Algebraic Structures for Transitive Closure.” <em>Theoretical Computer Science</em> 4 (1). Elsevier: 59–76.</p>
</div>
<div id="ref-oconnor2011shortestpaths">
<p>O’Connor, Russell. 2011. “A Very General Method for Computing Shortest Paths.” <a href="http://r6.ca/blog/20110808T035622Z.html" class="uri">http://r6.ca/blog/20110808T035622Z.html</a>.</p>
</div>
<div id="ref-penaloza2005transitive">
<p>Penaloza, Rafael. 2005. “Algebraic Structures for Transitive Closure.” <a href="http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.71.7650" class="uri">http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.71.7650</a>.</p>
</div>
</div>

