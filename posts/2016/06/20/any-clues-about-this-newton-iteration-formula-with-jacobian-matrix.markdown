---
katex: true
title: 'Any clues about this Newton iteration formula with Jacobian matrix?'
published: 2016-06-20T21:59:56Z
categories: math
tags: algorithm,Boltzmann,formula,iteration,Jacobian,Newton,sampling
---

<p><a href="https://byorgey.github.io/blog/posts/2016/03/23/boltzmann-sampling-for-generic-arbitrary-instances.html">A while ago I wrote about</a> using Boltzmann sampling to generate random instances of algebraic data types, and mentioned that I have some code I inherited for doing the core computations. There is <a href="https://github.com/byorgey/boltzmann/blob/master/gen/src/Oracle/Newton.hs#L52">one part of the code</a> that I still don’t understand, having to do with a variant of <a href="https://en.wikipedia.org/wiki/Newton&#039;s%20method">Newton’s method</a> for finding a fixed point of a mutually recursive system of equations. It seems to work, but I don’t like using code I don’t understand—for example, I’d like to be sure I understand the conditions under which it does work, to be sure I am not misusing it. I’m posting this in the hopes that someone reading this may have an idea.</p>
<p>Let $\Phi : \mathbb{R}^n \to \mathbb{R}^n$ be a vector function, defined elementwise in terms of functions $\Phi_1, \dots, \Phi_n : \mathbb{R}^n \to \mathbb{R}$:</p>
<p><div style="text-align:center;">
$\displaystyle \Phi(\mathbf{X}) = (\Phi_1(\mathbf{X}), \dots, \Phi_n(\mathbf{X}))$
</div></p>
<p>where $\mathbf{X} = (X_1, \dots, X_n)$ is a vector in $\mathbb{R}^n$. We want to find the fixed point $\mathbf{Y}$ such that $\Phi(\mathbf{Y}) = \mathbf{Y}$.</p>
<p>The algorithm (you can <a href="https://github.com/byorgey/boltzmann/blob/master/gen/src/Oracle/Newton.hs#L52">see the code here</a>) now works as follows. First, define $\mathbf{J}$ as the $n \times n$ <a href="https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant">Jacobian matrix</a> of partial derivatives of the $\Phi_i$, that is,</p>
<p><div style="text-align:center;">
$\displaystyle \displaystyle \mathbf{J} = \begin{bmatrix} \frac{\partial}{\partial X_1} \Phi_1 & \dots &
\frac{\partial}{\partial X_n} \Phi_1 \\ \vdots & \ddots & \vdots \\
\frac{\partial}{\partial X_1} \Phi_n & \dots &
\frac{\partial}{\partial X_n} \Phi_n\end{bmatrix}$
</div></p>
<p>Now let $\mathbf{Y}_0 = (0, \dots, 0)$ and let $\mathbf{U}_0 = I_n$ be the $n \times n$ identity matrix. Then for each $i \geq 0$ define</p>
<p><div style="text-align:center;">
$\displaystyle \mathbf{U}_{i+1} = \mathbf{U}_i + \mathbf{U}_i(\mathbf{J}(\mathbf{Y}_i)\mathbf{U}_i - (\mathbf{U}_i - I_n))$
</div></p>
<p>and also</p>
<p><div style="text-align:center;">
$\displaystyle \mathbf{Y}_{i+1} = \mathbf{Y}_i + \mathbf{U}_{i+1}(\Phi(\mathbf{Y}_i) - \mathbf{Y}_i).$
</div></p>
<p>Somehow, magically (under appropriate conditions on $\Phi$, I presume), the sequence of $\mathbf{Y}_i$ converge to the fixed point $\mathbf{Y}$. But I don’t understand where this is coming from, especially the equation for $\mathbf{U}_{i+1}$. Most generalizations of Newton’s method that I can find seem to involve multiplying by the <em>inverse</em> of the Jacobian matrix. So what’s going on here? Any ideas/pointers to the literature/etc?</p>
<div id="refs" class="references">

</div>

