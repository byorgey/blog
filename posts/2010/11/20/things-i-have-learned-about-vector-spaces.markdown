---
title: 'Things I have learned about vector spaces'
published: 2010-11-20T04:31:52Z
categories: math,projects
tags: affine,diagrams,inverse,normals,points,translation,transpose,vectors
---

Work on the <a href="http://trac.haskell.org/diagrams/">diagrams library</a> is coming along rather nicely.  I'll have more to say about it soon, but for now here are two things I have learned recently:

<ul>
	<li>Normals transform as the inverse transpose (<a href="http://www.faqs.org/faqs/graphics/algorithms-faq/">see Subject 5.27</a>).</li>
	<li>Be very careful about distinguishing between points and vectors.  Otherwise you are likely to get bitten by translations doing the Wrong Thing.</li>
</ul>



