---
katex: true
title: 'Competitive programming in Haskell: vectors and 2D geometry'
published: 2020-06-24T20:21:43Z
categories: competitive programming,haskell
tags: geometry,Kattis,robot,vector
---

<p>In my previous post (apologies it has been so long!) I challenged you to solve <a href="https://open.kattis.com/problems/vacuumba">Vacuumba</a>, which asks us to figure out where a robot ends up after following a sequence of instructions. Mathematically, this corresponds to adding up a bunch of vectors, but the interesting point is that the instructions are always <em>relative</em> to the robot’s current state, so robot programs are <em>imperative</em> programs.</p>
<h2 id="vector-basics">Vector basics</h2>
<p>The first order of business is to code up some primitives for dealing with (2D) vectors. I have accumulated a lot of library code for doing geometric stuff, but it’s kind of a mess; I’m using this as an opportunity to clean it up bit by bit. So there won’t be much code at first, but the library will grow as we do more geometry problems. The code so far (explained below) can be found <a href="https://github.com/byorgey/comprog-hs/blob/master/Geom.hs">in the comprog-hs repository</a>.</p>
<p>First, a basic representation for 2D vectors, the zero vector, and addition and subtraction of vectors.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:green;">{-# LANGUAGE GeneralizedNewtypeDeriving #-}</span>

<span style="color:blue;font-weight:bold;">module</span> <span>Geom</span> <span style="color:blue;font-weight:bold;">where</span>

<span style="color:green;">------------------------------------------------------------</span>
<span style="color:green;">-- 2D points and vectors</span>

<span style="color:blue;font-weight:bold;">data</span> <span>V2</span> <span>s</span> <span style="color:red;">=</span> <span>V2</span> <span>!</span><span>s</span> <span>!</span><span>s</span> <span style="color:blue;font-weight:bold;">deriving</span> <span style="color:red;">(</span><span>Eq</span><span style="color:red;">,</span> <span>Ord</span><span style="color:red;">,</span> <span>Show</span><span style="color:red;">)</span>
<span style="color:blue;font-weight:bold;">type</span> <span>V2D</span>  <span style="color:red;">=</span> <span>V2</span> <span>Double</span>

<span style="color:blue;font-weight:bold;">instance</span> <span>Foldable</span> <span>V2</span> <span style="color:blue;font-weight:bold;">where</span>
  <span>foldMap</span> <span>f</span> <span style="color:red;">(</span><span>V2</span> <span>x</span> <span>y</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>f</span> <span>x</span> <span>&lt;&gt;</span> <span>f</span> <span>y</span>

<span>zero</span> <span style="color:red;">::</span> <span>Num</span> <span>s</span> <span style="color:red;">=&gt;</span> <span>V2</span> <span>s</span>
<span>zero</span> <span style="color:red;">=</span> <span>V2</span> <span class="hs-num">0</span> <span class="hs-num">0</span>

<span style="color:green;">-- Adding and subtracting vectors</span>
<span style="color:red;">(</span><span>^+^</span><span style="color:red;">)</span><span style="color:red;">,</span> <span style="color:red;">(</span><span>^-^</span><span style="color:red;">)</span> <span style="color:red;">::</span> <span>Num</span> <span>s</span> <span style="color:red;">=&gt;</span> <span>V2</span> <span>s</span> <span style="color:red;">-&gt;</span> <span>V2</span> <span>s</span> <span style="color:red;">-&gt;</span> <span>V2</span> <span>s</span>
<span>V2</span> <span>x1</span> <span>y1</span> <span>^+^</span> <span>V2</span> <span>x2</span> <span>y2</span> <span style="color:red;">=</span> <span>V2</span> <span style="color:red;">(</span><span>x1</span><span>+</span><span>x2</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>y1</span><span>+</span><span>y2</span><span style="color:red;">)</span>
<span>V2</span> <span>x1</span> <span>y1</span> <span>^-^</span> <span>V2</span> <span>x2</span> <span>y2</span> <span style="color:red;">=</span> <span>V2</span> <span style="color:red;">(</span><span>x1</span><span style="color:green;">-</span><span>x2</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>y1</span><span style="color:green;">-</span><span>y2</span><span style="color:red;">)</span></code></pre>
<p>A few things to point out:</p>
<ul>
<li><p>The <code>V2</code> type is parameterized over the type of scalars, but we define <code>V2D</code> as a synonym for <code>V2 Double</code>, which is very common. The reason for making <code>V2</code> polymorphic in the first place, though, is that some problems require the use of exact integer arithmetic. It’s nice to be able to share code where we can, and have the type system enforce what we can and can’t do with vectors over various scalar types.</p></li>
<li><p>For a long time I just represented vectors as lists, <code>type V2 s =   [s]</code>. This makes implementing addition and subtraction very convenient: for example, <code>(^+^) = zipWith (+)</code>. Although this has worked just fine for solving many geometry problems, I have recently been reminded that having lots of small lists can be bad for performance. As long as we’re making a library anyway we might as well use a proper data type for vectors!</p></li>
<li><p><a href="https://vimeo.com/84249042">Elsewhere I have made a big deal out of the fact</a> that vectors and points ought to be represented as separate types. But in a competitive programming context I have always just used a single type for both and it hasn’t bit me (yet!).</p></li>
<li><p>The <code>Foldable</code> instance for <code>V2</code> gets us <code>toList</code>. It also gets us things like <code>sum</code> and <code>maximum</code> which could occasionally come in handy.</p></li>
</ul>
<h2 id="angles-and-rotation">Angles and rotation</h2>
<p>The other thing we are going to need for this problem is angles.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:green;">------------------------------------------------------------</span>
<span style="color:green;">-- Angles</span>

<span style="color:blue;font-weight:bold;">newtype</span> <span>Angle</span> <span style="color:red;">=</span> <span>A</span> <span>Double</span>  <span style="color:green;">-- angle (radians)</span>
  <span style="color:blue;font-weight:bold;">deriving</span> <span style="color:red;">(</span><span>Show</span><span style="color:red;">,</span> <span>Eq</span><span style="color:red;">,</span> <span>Ord</span><span style="color:red;">,</span> <span>Num</span><span style="color:red;">,</span> <span>Fractional</span><span style="color:red;">,</span> <span>Floating</span><span style="color:red;">)</span>

<span>fromDeg</span> <span style="color:red;">::</span> <span>Double</span> <span style="color:red;">-&gt;</span> <span>Angle</span>
<span>fromDeg</span> <span>d</span> <span style="color:red;">=</span> <span>A</span> <span style="color:red;">(</span><span>d</span> <span>*</span> <span>pi</span> <span>/</span> <span class="hs-num">180</span><span style="color:red;">)</span>

<span>fromRad</span> <span style="color:red;">::</span> <span>Double</span> <span style="color:red;">-&gt;</span> <span>Angle</span>
<span>fromRad</span> <span style="color:red;">=</span> <span>A</span>

<span>toDeg</span> <span style="color:red;">::</span> <span>Angle</span> <span style="color:red;">-&gt;</span> <span>Double</span>
<span>toDeg</span> <span style="color:red;">(</span><span>A</span> <span>r</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>r</span> <span>*</span> <span class="hs-num">180</span> <span>/</span> <span>pi</span>

<span>toRad</span> <span style="color:red;">::</span> <span>Angle</span> <span style="color:red;">-&gt;</span> <span>Double</span>
<span>toRad</span> <span style="color:red;">(</span><span>A</span> <span>r</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>r</span>

<span style="color:green;">-- Construct a vector in polar coordinates.</span>
<span>fromPolar</span> <span style="color:red;">::</span> <span>Double</span> <span style="color:red;">-&gt;</span> <span>Angle</span> <span style="color:red;">-&gt;</span> <span>V2D</span>
<span>fromPolar</span> <span>r</span> <span>θ</span> <span style="color:red;">=</span> <span>rot</span> <span>θ</span> <span style="color:red;">(</span><span>V2</span> <span>r</span> <span class="hs-num">0</span><span style="color:red;">)</span>

<span style="color:green;">-- Rotate a vector counterclockwise by a given angle.</span>
<span>rot</span> <span style="color:red;">::</span> <span>Angle</span> <span style="color:red;">-&gt;</span> <span>V2D</span> <span style="color:red;">-&gt;</span> <span>V2D</span>
<span>rot</span> <span style="color:red;">(</span><span>A</span> <span>θ</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>V2</span> <span>x</span> <span>y</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span>V2</span> <span style="color:red;">(</span><span>cos</span> <span>θ</span> <span>*</span> <span>x</span> <span style="color:green;">-</span> <span>sin</span> <span>θ</span> <span>*</span> <span>y</span><span style="color:red;">)</span> <span style="color:red;">(</span><span>sin</span> <span>θ</span> <span>*</span> <span>x</span> <span>+</span> <span>cos</span> <span>θ</span> <span>*</span> <span>y</span><span style="color:red;">)</span>
</code></pre>
<p>Nothing too complicated going on here: we have a type to represent angles, conversions to and from degrees and radians, and then two uses for angles: a function to construct a vector in polar coordinates, and a function to perform rotation.</p>
<p>Incidentally, one could of course define <code>type Angle = Double</code>, which would be simpler in some ways, but after getting bitten several times by forgetting to convert from degrees to radians, I decided it was much better to use a <code>newtype</code> and entirely prevent that class of error.</p>
<h2 id="solving-vacuumba">Solving Vacuumba</h2>
<p>Now we just put the pieces together to solve the problem. First, some imports:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:green;">{-# LANGUAGE FlexibleContexts #-}</span>
<span style="color:green;">{-# LANGUAGE RecordWildCards  #-}</span>

<span style="color:blue;font-weight:bold;">import</span>           <span>Control.Arrow</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Control.Monad.State</span>
<span style="color:blue;font-weight:bold;">import</span> <span style="color:blue;font-weight:bold;">qualified</span> <span>Data.Foldable</span>       <span style="color:blue;font-weight:bold;">as</span> <span>F</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Text.Printf</span>

<span style="color:blue;font-weight:bold;">import</span>           <span>Geom</span>
<span style="color:blue;font-weight:bold;">import</span>           <span>Scanner</span></code></pre>
<p>We make a data type for representing robot instructions, and a corresponding <code>Scanner</code>. Notice how we are forced to use <code>fromDeg</code> to convert the raw input into an appropriate type.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:blue;font-weight:bold;">data</span> <span>Instr</span> <span style="color:red;">=</span> <span>I</span> <span style="color:red;">{</span> <span>turn</span> <span style="color:red;">::</span> <span>Angle</span><span style="color:red;">,</span> <span>dist</span> <span style="color:red;">::</span> <span>Double</span> <span style="color:red;">}</span>

<span>instr</span> <span style="color:red;">::</span> <span>Scanner</span> <span>Instr</span>
<span>instr</span> <span style="color:red;">=</span> <span>I</span> <span>&lt;$&gt;</span> <span style="color:red;">(</span><span>fromDeg</span> <span>&lt;$&gt;</span> <span>double</span><span style="color:red;">)</span> <span>&lt;*&gt;</span> <span>double</span></code></pre>
<p>The high-level solution then reads the input via a <code>Scanner</code>, solves each scenario, and formats the output. The output is a <code>V2D</code>, so we just convert it to a list with <code>F.toList</code> and use <a href="https://hackage.haskell.org/package/base-4.14.0.0/docs/Text-Printf.html"><code>printf</code></a> to format each coordinate.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>main</span> <span style="color:red;">=</span> <span>interact</span> <span>$</span>
  <span>runScanner</span> <span style="color:red;">(</span><span>numberOf</span> <span style="color:red;">(</span><span>numberOf</span> <span>instr</span><span style="color:red;">)</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span>
  <span>map</span> <span style="color:red;">(</span><span>solve</span> <span>&gt;&gt;&gt;</span> <span>F.toList</span> <span>&gt;&gt;&gt;</span> <span>map</span> <span style="color:red;">(</span><span>printf</span> <span style="color:teal;">"%.6f"</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span> <span>unwords</span><span style="color:red;">)</span> <span>&gt;&gt;&gt;</span> <span>unlines</span></code></pre>
<p>Our <code>solve</code> function needs to take a list of instructions, and output the final location of the robot. Since the instructions can be seen as an imperative program for updating the state of the robot, it’s entirely appropriate to use a localized <code>State</code> computation.</p>
<p>First, a data type to represent the robot’s current state, consisting of a 2D vector recording the position, and an angle to record the current heading. <code>initRS</code> records the robot’s initial state (noting that it starts out facing north, corresponding to an angle of $90^\circ$ as measured clockwise from the positive $x$-axis).</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:blue;font-weight:bold;">data</span> <span>RobotState</span> <span style="color:red;">=</span> <span>RS</span> <span style="color:red;">{</span> <span>pos</span> <span style="color:red;">::</span> <span>V2D</span><span style="color:red;">,</span> <span>heading</span> <span style="color:red;">::</span> <span>Angle</span> <span style="color:red;">}</span>
<span>initRS</span> <span style="color:red;">=</span> <span>RS</span> <span>zero</span> <span style="color:red;">(</span><span>fromDeg</span> <span class="hs-num">90</span><span style="color:red;">)</span></code></pre>
<p>Finally, the <code>solve</code> function itself executes each instruction in sequence as a <code>State RobotState</code> computation, uses <code>execState</code> to run the resulting overall computation and extract the final state, and then projects out the robot’s final position. Executing a single instruction is where the geometry happens: we look up the current robot state, calculate its new heading by adding the turn angle to the current heading, construct a movement vector in the direction of the new heading using polar coordinates, and add the movement to the current position.</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span>solve</span> <span style="color:red;">::</span> <span style="color:red;">[</span><span>Instr</span><span style="color:red;">]</span> <span style="color:red;">-&gt;</span> <span>V2D</span>
<span>solve</span> <span style="color:red;">=</span> <span>mapM_</span> <span>exec</span> <span>&gt;&gt;&gt;</span> <span>flip</span> <span>execState</span> <span>initRS</span> <span>&gt;&gt;&gt;</span> <span>pos</span>
  <span style="color:blue;font-weight:bold;">where</span>
    <span>exec</span> <span style="color:red;">::</span> <span>Instr</span> <span style="color:red;">-&gt;</span> <span>State</span> <span>RobotState</span> <span>()</span>
    <span>exec</span> <span style="color:red;">(</span><span>I</span> <span>θ</span> <span>d</span><span style="color:red;">)</span> <span style="color:red;">=</span> <span style="color:blue;font-weight:bold;">do</span>
      <span>RS</span><span style="color:red;">{</span><span style="color:red;">..</span><span style="color:red;">}</span> <span style="color:red;">&lt;-</span> <span>get</span>
      <span style="color:blue;font-weight:bold;">let</span> <span>heading'</span> <span style="color:red;">=</span> <span>heading</span> <span>+</span> <span>θ</span>
          <span>move</span>     <span style="color:red;">=</span> <span>fromPolar</span> <span>d</span> <span>heading'</span>
      <span>put</span> <span>$</span> <span>RS</span> <span style="color:red;">(</span><span>pos</span> <span>^+^</span> <span>move</span><span style="color:red;">)</span> <span>heading'</span></code></pre>
<h2 id="for-next-time">For next time</h2>
<p>We’ll definitely be doing more geometry, but for the next post I feel like doing something different. I invite you to solve <a href="https://open.kattis.com/problems/checkingbreak">Checking Break</a>.</p>

