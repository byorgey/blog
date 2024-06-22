---
katex: true
title: 'Average and median via optimization'
published: 2021-06-08T16:07:47Z
categories: math
tags: absolute value,average,derivative,median,optimization
---

<p><em>This is certainly not a new observation, and I’m sure it is folklore and/or contained in various textbooks, but it was amusing to derive it independently.</em></p>
<p>Suppose we have a finite set of real numbers $\{x_1, \dots, x_n\}$, and we want to pick a value $m$ which is somehow “in the middle” of the $x_i$. The punchline is that</p>
<ul>
<li>if we want to minimize the sum of the <em>squared</em> distances from $m$ to each $x_i$, we should pick $m$ to be the <em>average</em> of the $x_i$;</li>
<li>if we want to minimize the sum of the <em>absolute</em> distances from $m$ to each $x_i$, we should pick $m$ to be the <em>median</em> of the $x_i$.</li>
</ul>
<p>The first of these is tricky to understand intuitively but easy to derive; the second is intuitively straightforward but trying to derive it leads to an interesting twist.</p>
<h2 id="average-minimizing-sum-of-squared-distances">Average = minimizing sum of squared distances</h2>
<p>Let’s not worry about <em>why</em> we would want to minimize the sum of <em>squared</em> distances; there are good reasons and it’s not the point. I don’t know about you, but I find it difficult to reason intuitively about how and why to pick $m$ to minimize this sum of squared differences. If you know of an intuitive way to explain this, I would love to hear about it! But in any case, it is easy to derive using some strightforward calculus.</p>
<p>Let $\displaystyle S(m) = \sum_i(m - x_i)^2$ denote the sum of squared distances from a given $m$ to each of the $x_i$. Taking the derivative of $S$ with respect to $m$, we find</p>
<div style="text-align:center;">
<p>$\displaystyle \frac{d}{dm} S(m) = \sum_i 2(m - x_i)$.</p>
</div>
<p>Setting the derivative equal to zero, we can first divide through by the factor of 2, yielding</p>
<div style="text-align:center;">
<p>$\displaystyle 0 = \sum_i (m - x_i)$</p>
</div>
<p>Since $m$ does not depend on $i$, this is just $n$ copies of $m$ less the sum of the $x_i$. Hence, solving for $m$ yields</p>
<div style="text-align:center;">
<p>$\displaystyle m = \frac{1}{n} \sum_i x_i$</p>
</div>
<p>as expected: the value of $m$ which minimizes the sum of squared distances to the $x_i$ is their average, that is, the sum of the $x_i$ divided by the size of the set.</p>
<h2 id="median-minimizing-sum-of-absolute-distances">Median = minimizing sum of absolute distances</h2>
<p>Now suppose we want to minimize the sum of <em>absolute</em> distances instead, that is,</p>
<div style="text-align:center;">
<p>$S(m) = \sum_i |m - x_i|$</p>
</div>
<p>In this scenario, it is much easier to reason out the correct answer. Start with some arbitrary $m$, and imagine nudging it by some small amount $\Delta x$, say, to the right. $m$’s distances to any points on its left will each increase by $\Delta x$, and its distances to any points on its right will each decrease by the same amount. Therefore, if there are more $x_i$ to the left of $m$, then the overall sum of distances distances will increase; if there are more $x_i$ to the right, then the overall sum will decrease. So, to find $m$ which minimizes the sum of absolute differences, we want the same number of $x_i$ on the left and the right, that is, we want the <em>median</em>. Note that if $n$ is odd, then we must pick $m$ to be exactly equal to the $x_i$ in the middle; if $n$ is even, then we can pick $m$ to be anywhere inside the interval between the middle two $x_i$.</p>
<p>Just for fun, can we derive this answer using calculus, like we did for minimizing squared differences? There is a wrinkle, of course, which is that the absolute value function is not differentiable everywhere: it has a sharp corner at zero. But we won’t let that stop us! Clearly the derivative of $|x|$ is $-1$ when $x < 0$ and $1$ when $x > 0$. So it seems reasonable to just assign the derivative a value of $0$ at $x = 0$. Algebraically, we can define</p>
<div style="text-align:center;">
<p>$\displaystyle \frac{d}{dx} |x| = [x > 0] - [x < 0]$</p>
</div>
<p>where $[P]$ is equal to $1$ when the proposition $P$ is true, and $0$ when it is false (this notation is called the <a href="https://en.wikipedia.org/wiki/Iverson_bracket"><em>Iverson bracket</em></a>). So when $x > 0$ we get $[x > 0] - [x < 0] = 1 - 0 = 1$; when $x < 0$ we get $0 - 1 = -1$; and when $x = 0$ both propositions are false so we get $0 - 0 = 0$.</p>
<p>Armed with this definition, we can differentiate $S$ with respect to $m$:</p>
<div style="text-align:center;">
<p>$\displaystyle \frac{d}{dm} S(m) = \frac{d}{dm} \sum_i |m - x_i| = \sum_i [m > x_i] - \sum_i [m < x_i]$</p>
</div>
<p>Clearly, this is zero when $\displaystyle \sum_i [m > x_i] = \sum_i [m < x_i]$, that is, when there are the same number of $x_i$ on either side of $m$.</p>
<p>The curious thing to me is that even though the derivative of $|x|$ is undefined when $x = 0$, it seems like it “wants” to be 0 here. In general, if we assign the value $k$ to the derivative at $x = 0$, the derivative of $S$ becomes</p>
<div style="text-align:center;">
<p>$\displaystyle \frac{d}{dm} S(m) = \sum_i [m > x_i] + k \sum_i [m = x_i] - \sum_i [m < x_i]$</p>
</div>
<p>When $k$ is nonzero and $n$ is odd, there are no values of $m$ for which this derivative is zero, making it more difficult to find the minimum.</p>

