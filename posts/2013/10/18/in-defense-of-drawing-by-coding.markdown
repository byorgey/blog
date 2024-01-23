---
title: In defense of drawing by coding
published: 2013-10-18T18:39:09Z
categories: 
tags: data,diagrams,drawing,graphical,illustration,interface,programming,visualization
---

<p>Just some half-baked thoughts in repsonse to <a href="http://worrydream.com/">Bret Victor’s</a> talk, <a href="http://vimeo.com/66085662">Drawing Dynamic Visualizations</a> (along with his <a href="http://worrydream.com/DrawingDynamicVisualizationsTalkAddendum/">addendum here</a>). As usual, it is a fun and inspiring talk, so if you haven’t seen it I highly recommend it; but I will summarize it here.</p>
<p>He starts by surveying the state-of-the-art in options for the creative scientist who wants to visualize some data. The options he outlines are:</p>
<ol style="list-style-type:decimal;">
<li><p>Use some program like Excel which has a standard repertoire of graphs it can generate. The problem with this approach is that it completely stifles any creativity and freedom in visualizing data.</p></li>
<li><p>Use a drawing program like Illustrator or Inkscape. This gives more freedom, of course, but the process is tedious and the results cannot easily be modified.</p></li>
<li><p>The final option is to write some code in a framework like <a href="http://www.processing.org/">Processing</a> or <a href="http://d3js.org/">d3.js</a>. The problem here, Victor says, is that you are just staring at a mass of symbols with no immediate, dynamic feedback.</p></li>
</ol>
<p>He then goes on to demo a really cool prototype tool that allows drawing using a graphical interface, a bit like Illustrator or Inkscape. But the similarity is only surface deep: where those programs are restrictive and inflexible, Victor’s is richly interactive and editable. Instead of drawing concretely located lines, circles, and so on, it infers the <em>relationships</em> between things you are drawing, so updating the characteristics or positioning of one element automatically updates all the others which depend on it as well. In other words, one can construct a <em>generic</em>, <em>editable</em> visualization just by drawing one particular <em>example</em> of it.</p>
<p>Victor is quite negative about option (3) above—drawing by coding—referring to programming as “blindly manipulating symbols”: “blind” because you can’t actually see the picture you are creating while writing the program.</p>
<p>What I would like to point out is that in fact, despite his negativity about drawing by programming, when using his graphical tool Victor is <em>still programming</em>! It’s just that he has a graphical interface which allows him to construct certain sorts of programs, instead of writing the programs directly. In fact, you can see the programs he constructs on the left side of the screen in his tool. They appear to be structured imperative programs, consisting of sequences of drawing instructions together with things like loops and conditionals.</p>
<p>The problem is that this kind of higher-level interface cannot provide for all possible circumstances (unless you somehow make it Turing complete, but in that case it probably ceases to be at all intuitive). For example, Victor impressively drags and drops some spreadsheet data into his application. But what if I want to use data which is structured in some other format? What if I need to preprocess the data in some computationally nontrivial way? Or on the drawing end, what if I want to draw some shapes or compute some positions in a way that the interface does not provide for? We can’t completely get away from the need to write code in the service of visualization.</p>
<p>What we really need is a more inclusive idea of “programming”, and a continuum between direct manipulation of images and manipulation of symbols to produce images. Symbolic methods, of course, can be incredibly powerful—there is nothing <em>inherently</em> wrong with manipulating symbols.</p>
<p>More specifically, I am proposing something like the following:</p>
<ol style="list-style-type:decimal;">
<li><p>First, I am all for making elegant and powerful high-level graphical interfaces for constructing interactive, editable drawings—and not just drawings but <em>code</em> that generates drawings. This can probably be pushed quite far, and there is lots of HCI research to be done here. There are also some very interesting questions relating to bidirectional computation here: making an edit via the graphical interface corresponds to some sort of edit to the code; how can this be done in a sensible and consistent way?</p></li>
<li><p>Recognizing, however, that sometimes you do need to actually write some code, how can we make the underlying language as beautiful as possible, and how can we make the interaction between the two systems (code and higher-level graphical interface) as elegant and seamless as possible? The ideal is for a user to be able to flow easily back and forth between the two modes, ideally spending much of their time in a high-level graphical mode.</p></li>
</ol>
<p>Of course, if you hadn’t guessed by now, in the long term this is the sort of direction I would love to go with <a href="http://projects.haskell.org/diagrams">diagrams</a>… though I need to finish my dissertation first (more on that subject soon).</p>
<div class="references">

</div>

