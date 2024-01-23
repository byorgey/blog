---
title: Competitive programming in Haskell: Infinite 2D array, Level 1
published: 2022-12-03T11:43:08Z
categories: competitive programming,haskell
tags: Kattis,number
---

<p>In my <a href="https://byorgey.wordpress.com/2022/09/01/competitive-programming-in-haskell-infinite-2d-array/">previous post</a>, I challenged you to solve <a href="https://open.kattis.com/problems/infinite2darray">Infinite 2D Array</a> using Haskell. As a reminder, the problem specifies a two-parameter recurrence $latex F_{x,y}$, given by</p>
<ul>
<li>$latex F_{0,0} = 0$</li>
<li>$latex F_{0,1} = F_{1,0} = 1$</li>
<li>$latex F_{i,0} = F_{i-1,0} + F_{i-2,0}$ for $latex i \geq 2$</li>
<li>$latex F_{0,i} = F_{0,i-1} + F_{0,i-2}$ for $latex i \geq 2$</li>
<li>$latex F_{i,j} = F_{i-1,j} + F_{i,j-1}$ for $latex i,j \geq 1$.</li>
</ul>
<p>We are given particular values of $latex x$ and $latex y$, and asked to compute $latex F_{x,y} \bmod (10^9 + 7)$. The problem is that $latex x$ and $latex y$ could be as large as $latex 10^6$, so simply computing the entire $latex x \times y$ array is completely out of the question: it would take almost 4 <em>terabytes</em> of memory to store a $latex 10^6 \times 10^6$ array of 32-bit integer values. In this post, I’ll answer the Level 1 challenge: coming up with a general formula for $latex F_{x,y}$.</p>
<p>We need to be more clever about computing a given $latex F_{x,y}$ without computing every entry in the entire 2D array, so we look for some patterns. It’s pretty obvious that the array has Fibonacci numbers along both the top two rows and the first two columns, though it’s sadly just as obvious that we don’t get Fibonacci numbers anywhere else. The last rule, the rule that determines the interior entries, says that each interior cell is the sum of the cell above it and the cell to the left. This looks a lot like the rule for generating Pascal’s triangle, <em>i.e.</em> binomial coefficients; in fact, if the first row and column were specified to be all 1’s instead of Fibonacci numbers, then we would get exactly binomial coefficients.</p>
<p>I knew that binomial coefficients can also be thought of as counting <a href="http://discrete.openmathbooks.org/dmoi2/sec_counting-binom.html">the number of paths from one point in a grid to another which can only take east or south steps</a>, and this finally gave me the right insight. Each interior cell is a sum of other cells, which are themselves sums of other cells, and so on until we get to the edges, and so ultimately each interior cell can be thought of as a sum of a bunch of copies of numbers on the edges, <em>i.e.</em> Fibonacci numbers. How many copies? Well, the number of times each Fibonacci number on an edge contributes to a particular interior cell is equal to the number of paths from the Fibonacci number to the interior cell (with the restriction that the paths’ first step must immediately be into the interior of the grid, instead of taking a step along the first row or column). For example, consider $latex F_{3,2} = 11$. The two 1’s along the top row contribute 3 times and 1 time, respectively, whereas the 1’s and 2 along the first column contribute 3 times, 2 times, and once, respectively, for a total of $latex 11$:</p>
<div style="text-align: center">
<p><img src="http://byorgey.files.wordpress.com/2022/12/331a85cd2f470b8c.png" /></p>
</div>
<p>The number of paths from $latex F_{0,k}$ to $latex F_{x,y}$ is the number of grid paths from $latex (1,k)$ to $latex (x,y)$, which is $latex \binom{(x-1) + (y-k)}{y-k}$. Likewise the number of paths from $latex F_{k,0}$ to $latex F_{x,y}$ is $latex \binom{(x-k) + (y-1)}{x-k}$. All together, this yields the formula</p>
<p>$latex \displaystyle F_{x,y} = \left(\sum_{1 \leq k \leq x} F_k \binom{x-k+y-1}{x-k}\right) + \left(\sum_{1 \leq k \leq y} F_k \binom{y-k+x-1}{y-k}\right) \pmod{P}$</p>
<p>Commenter <a href="https://byorgey.wordpress.com/2022/09/01/competitive-programming-in-haskell-infinite-2d-array/#comment-40784">Soumik Sarkar found a different formula</a>,</p>
<p>$latex \displaystyle F_{x,y} = F_{x+2y} + \sum_{1 \leq k \leq y} (F_k - F_{2k}) \binom{y-k+x-1}{y-k} \pmod{P}$</p>
<p>which clearly has some similarity to mine, but I have not been able to figure out how to derive it, and Soumik did not explain how they found it. Any insights welcome!</p>
<p>In any case, both of these formulas involve a sum of only $latex O(x+y)$ terms, instead of $latex O(xy)$, although the individual terms are going to be much more work to compute. The question now becomes how to efficiently compute Fibonacci numbers and binomial coefficients modulo a prime. I’ll talk about that in the next post!</p>

