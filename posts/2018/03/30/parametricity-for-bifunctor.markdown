---
title: Parametricity for Bifunctor
published: 2018-03-31T03:12:25Z
categories: haskell,math
tags: Bifunctor,bimap,free,parametricity,Typeclassopedia
---

<p>I’ve begun work to add <a href="http://hackage.haskell.org/package/base-4.11.0.0/docs/Data-Bifunctor.html"><code>Bifunctor</code></a>, <a href="http://hackage.haskell.org/package/base-4.11.0.0/docs/Data-Bifunctor.html"><code>Bifoldable</code></a>, and <a href="http://hackage.haskell.org/package/base-4.11.0.0/docs/Data-Bitraversable.html"><code>Bitraversable</code></a> to the <a href="https://wiki.haskell.org/Typeclassopedia">Typeclassopedia</a>. While thinking and writing about <code>Bifunctor</code> I ended up proving some “folklore” results for my own satisfaction, and decided someone might possibly find it useful if I formally record the proofs here.</p>
<h2 id="bifunctor-and-its-laws">Bifunctor and its laws</h2>
<p>The <code>Bifunctor</code> type class governs types of kind <code>* -&gt; * -&gt; *</code> which are (covariantly) functorial in both type arguments; that is, we can map over the two type arguments independently:</p>
<pre class="sourceCode haskell"><code class="sourceCode haskell"><span style="color:blue;font-weight:bold;">class</span> <span>Bifunctor</span> <span style="color:red;">(</span><span>p</span> <span style="color:red;">::</span> <span>*</span> <span style="color:red;">-&gt;</span> <span>*</span> <span style="color:red;">-&gt;</span> <span>*</span><span style="color:red;">)</span> <span style="color:blue;font-weight:bold;">where</span>
  <span>bimap</span>  <span style="color:red;">::</span> <span style="color:red;">(</span><span>a</span> <span style="color:red;">-&gt;</span> <span>b</span><span style="color:red;">)</span> <span style="color:red;">-&gt;</span> <span style="color:red;">(</span><span>c</span> <span style="color:red;">-&gt;</span> <span>d</span><span style="color:red;">)</span> <span style="color:red;">-&gt;</span> <span>p</span> <span>a</span> <span>c</span> <span style="color:red;">-&gt;</span> <span>p</span> <span>b</span> <span>d</span>

  <span>first</span>  <span style="color:red;">::</span> <span style="color:red;">(</span><span>a</span> <span style="color:red;">-&gt;</span> <span>b</span><span style="color:red;">)</span> <span style="color:red;">-&gt;</span> <span>p</span> <span>a</span> <span>c</span> <span style="color:red;">-&gt;</span> <span>p</span> <span>b</span> <span>c</span>
  <span>second</span> <span style="color:red;">::</span> <span style="color:red;">(</span><span>b</span> <span style="color:red;">-&gt;</span> <span>c</span><span style="color:red;">)</span> <span style="color:red;">-&gt;</span> <span>p</span> <span>a</span> <span>b</span> <span style="color:red;">-&gt;</span> <span>p</span> <span>a</span> <span>c</span></code></pre>
<p><code>bimap</code> allows mapping over both type arguments simultaneously, for example, <code>bimap toUpper (+1) ('j', 3) = ('J',4)</code>. <code>first</code> and <code>second</code> allow mapping over only one or the other. Note that <code>bimap</code> can be implemented in terms of <code>first</code> and <code>second</code>, or <code>first</code> and <code>second</code> in terms of <code>bimap</code> (we’ll see what these implementations are later, or you can work them out as an easy exercise if you haven’t seen them before).</p>
<p>Instances of <code>Bifunctor</code> should satisfy some laws, which are straightforward analogues of the laws for <code>Functor</code> instances:</p>
<pre><code>bimap id id = id                                (IB)
bimap (f . g) (h . i) = bimap f h . bimap g i   (CB)

first id = id                                   (IF)
first (f . g) = first f . first g               (CF)

second id = id                                  (IS)
second (f . g) = second f . second g            (CS)</code></pre>
<p>So far so good. But there is an additional law relating <code>bimap</code>, <code>first</code>, and <code>second</code>:</p>
<pre><code>bimap f g = first f . second g                  (BFS)</code></pre>
<p>When I first read this law, it gave me pause: unlike the other laws, there seems to be some arbitrary asymmetry here. What about the symmetric law BFS2?</p>
<pre><code>bimap f g = second g . first f                  (BFS2)</code></pre>
<p>Intuitively, the order in which we perform <code>first f</code> and <code>second g</code> should not make any difference, since they cannot interact with each other in any way; the <code>a</code> values and <code>b</code> values in an arbitrary <code>p a b</code> structure have to be distinct. That is, intuitively, if (BFS) holds, then (BFS2) should hold as well, and I wondered whether it is possible to formally derive this fact.</p>
<p>Let us note first that if one defines a <code>Bifunctor</code> instance by defining <code>bimap</code> alone, then <code>first</code> and <code>second</code> receive default definitions:</p>
<pre><code>first  f = bimap f id
second g = bimap id g</code></pre>
<p>In this case, assuming that <code>bimap</code> satisfies its composition law (CB), we can see that</p>
<pre><code>  first f . second g
= bimap f id . bimap id g
= bimap (f . id) (id . g)
= bimap f g
= bimap (id . f) (g . id)
= bimap id g . bimap f id
= second g . first f</code></pre>
<p>So when <code>bimap</code> alone is defined and satisfies (CB), both (BFS) and (BFS2) hold automatically. Let’s now consider the situation where <code>first</code> and <code>second</code> are defined, and <code>bimap</code> acquires its default definition of <code>bimap f g = first f . second g</code> (which is just (BFS))—or we can assume that all three of <code>first</code>, <code>second</code>, and <code>bimap</code> are defined, as long as law (BFS) still holds. In this case, can we derive (BFS2) as well?</p>
<h2 id="parametricity-for-bifunctor">Parametricity for Bifunctor</h2>
<p>We can derive parametricity, aka a <a href="https://people.mpi-sws.org/~dreyer/tor/papers/wadler.pdf">free theorem</a>, for any function with the type of <code>bimap</code>. Let <code>p :: * -&gt; * -&gt; *</code> be some two-argument type. In order to even express parametricity for types involving <code>p</code>, we have to assume that <code>p</code> is functorial—that is, we assume the existence of a function</p>
<pre><code>BIMAP :: (a -&gt; b) -&gt; (c -&gt; d) -&gt; p a c -&gt; p b d</code></pre>
<p>satisfying (IB). (As with <code>Functor</code>, (IB) + parametricity is enough to imply that it must satisfy (CB) as well.) We write <code>BIMAP</code> in all caps to indicate that it is some canonical, Platonic function, handed down from on high, inherent to the very essence of <code>p</code>. (Something like that.) Now let us suppose that we mortals have written some function <code>bimap</code> with the same type as <code>BIMAP</code>, which also satisfies (IB). But we don’t make any assumptions about whether our <code>bimap</code> is the same function as <code>BIMAP</code>; we only assume that it has type <code>(a -&gt; b) -&gt; (c -&gt; d) -&gt; p a c -&gt; p b d</code> and satisfies (IB).</p>
<p>We can now take the polymorphic type of <code>bimap</code> and turn the crank to derive a statement of parametricity. I’m assuming some familiarity with this process; for details, see <a href="https://people.mpi-sws.org/~dreyer/tor/papers/wadler.pdf">Wadler’s paper</a>. If you don’t care about the details of the derivation, you can just skip to the final statement of parametricity at the end of this section, and then proceed to see how it is applied in the next.</p>
<p>Parametricity for <code>bimap</code> says that if we interpret its type as a relation (according to certain rules), then <code>(bimap, bimap)</code> will be a member of the relation, that is, <code>bimap</code> will be related to itself. Below I show the process of expanding out the definition of the relation corresponding to <code>bimap</code>’s type. (I don’t want to take the time to typeset it really nicely using LaTeX or anything like that; a slightly cleaned-up text version will do for now.) I use <code>@a</code> to denote explicit type application, but after the second step I leave type application implicit to reduce clutter.</p>
<pre><code>(bimap,bimap) ∈ ∀ a b c d. (a -&gt; b) -&gt; (c -&gt; d) -&gt; p a c -&gt; p b d

&lt;=&gt;

∀ Q :: a &lt;-&gt; a', R :: b &lt;-&gt; b', S :: c &lt;-&gt; c', T :: d &lt;-&gt; d'.
  (bimap @a @b @c @d, bimap @a' @b' @c' @d')
    ∈ (Q -&gt; R) -&gt; (S -&gt; T) -&gt; p Q S -&gt; p R T

&lt;=&gt;

∀ Q :: a &lt;-&gt; a', R :: b &lt;-&gt; b', S :: c &lt;-&gt; c', T :: d &lt;-&gt; d'.
  ∀ (g,g') ∈ (Q -&gt; R).
    ∀ (h,h') ∈ (S -&gt; T).
      ∀ (x,x') ∈ p Q S.
        (bimap g h x, bimap g' h' x') ∈ p R T

&lt;=&gt;

∀ Q :: a &lt;-&gt; a', R :: b &lt;-&gt; b', S :: c &lt;-&gt; c', T :: d &lt;-&gt; d'.
  ∀ (g,g'). (∀ (y,y') ∈ Q. (g y, g' y') ∈ R) -&gt;
    ∀ (h,h'). (∀ (z,z') ∈ S. (h z, h' z') ∈ T) -&gt;
      ∀ (x,x') ∈ p Q S.
        (bimap g h x, bimap g' h' x') ∈ p R T</code></pre>
<p>At this point, we don’t need the extra generality afforded by assuming everything is a <em>relation</em>, so we specialize all the relations in sight to functions. For example, the relation <code>Q :: a &lt;-&gt; a'</code> just becomes a function <code>q :: a -&gt; a'</code>; statements of relatedness, such as <code>(y,y') ∈ Q</code>, turn into equations like <code>q y = y'</code>.</p>
<pre><code>∀ q :: a -&gt; a', r :: b -&gt; b', s :: c -&gt; c', t :: d -&gt; d'.
  ∀ (g,g'). (∀ (y,y'). q y = y' -&gt; r (g y) = g' y') -&gt;
    ∀ (h,h'). (∀ (z,z'). s z = z' -&gt; t (h z) = h' z') -&gt;
      ∀ (x,x') ∈ p q s.
        (bimap g h x, bimap g' h' x') ∈ p r t</code></pre>
<p>We now have to interpret <code>p q s</code> and <code>p r t</code> as relations. Wadler’s original paper doesn’t really cover this; it only talks about the specific case of the list functor. However, reading between the lines (and consulting subsequent work on parametricity), we can see that the right way to do this is</p>
<p><code>(a,b) ∈ p f g  &lt;=&gt;  BIMAP f g a = b</code></p>
<p>That is, we interpret the type <code>p</code> as the relation corresponding to its <code>BIMAP</code> function. Hence, we now have</p>
<pre><code>∀ q :: a -&gt; a', r :: b -&gt; b', s :: c -&gt; c', t :: d -&gt; d'.
  ∀ (g,g'). (∀ (y,y'). q y = y' -&gt; r (g y) = g' y') -&gt;
    ∀ (h,h'). (∀ (z,z'). s z = z' -&gt; t (h z) = h' z') -&gt;
      ∀ (x,x'). (BIMAP q s x = x') -&gt;
        BIMAP r t (bimap g h x) = bimap g' h' x'</code></pre>
<p>Finally, we can simplify this by substituting equals for equals in a few places:</p>
<pre><code>∀ q :: a -&gt; a', r :: b -&gt; b', s :: c -&gt; c', t :: d -&gt; d'.
  ∀ (g,g'). (∀ (y,y'). r (g y) = g' (q y)) -&gt;
    ∀ (h,h'). (∀ (z,z'). t (h z) = h' (s z)) -&gt;
      ∀ (x,x').
        BIMAP r t (bimap g h x) = bimap g' h' (BIMAP q s x)       (PAR)</code></pre>
<p>This is the specific statement of parametricity for <code>bimap</code> that we will use to derive the results below.</p>
<h2 id="uniquness-of-bimap">Uniquness of <code>bimap</code></h2>
<p>Let’s first prove that <code>bimap</code> is unique—that is, anything with the type of <code>bimap</code> must in fact be equal to <code>BIMAP</code>. The name of the game is just to pick appropriate values for all those quantified variables in (PAR). In particular, let’s pick</p>
<ul>
<li><code>g = h = q = s = id</code>,</li>
<li><code>r = g'</code>, and</li>
<li><code>t = h'</code>.</li>
</ul>
<p>We have some conditions to check:</p>
<ul>
<li><code>(∀ (y,y'). r (g y) = g' (q y))</code> under our choices becomes <code>(∀ (y,y'). r (id y) = r (id y))</code> which is obviously true.</li>
<li>Likewise, <code>(∀ (z,z'). t (h z) = h' (s z))</code> becomes <code>(∀ (z,z'). t (id z) = t (id z))</code> which is also obviously true.</li>
</ul>
<p>We then get to conclude that <code>∀ (x,x'). BIMAP r t (bimap g h x) = bimap g' h' (BIMAP q s x)</code>, which is equivalent to</p>
<pre><code>∀ (x,x'). BIMAP r t (bimap g h x) = bimap g' h' (BIMAP q s x)

&lt;=&gt;

BIMAP r t . bimap g h = bimap g' h' . BIMAP q s

&lt;=&gt;

BIMAP r t . bimap id id = bimap r t . BIMAP id id</code></pre>
<p>Since we have assumed that both <code>BIMAP</code> and <code>bimap</code> satisfy the <code>id</code> law, this is equivalent to <code>BIMAP r t = bimap r t</code>, so <code>bimap</code> and <code>BIMAP</code> coincide (<code>r</code> and <code>t</code> were arbitrary).</p>
<h2 id="first-and-second-commute"><code>first</code> and <code>second</code> commute</h2>
<p>Finally, let’s prove that <code>first</code> and <code>second</code> commute. We will again use (PAR), but with different choices:</p>
<ul>
<li><code>g = g' = t = s = id</code></li>
<li><code>h = h'</code></li>
<li><code>q = r</code></li>
</ul>
<p>Let’s check the conditions:</p>
<ul>
<li>We have to check<code>(∀ (y,y'). r (g y) = g' (q y))</code>, which becomes <code>(∀ (y,y'). r (id y) = id (r y))</code>, which is true since both sides reduce to <code>r y</code>.</li>
<li>Likewise, <code>(∀ (z,z'). t (h z) = h' (s z))</code> becomes <code>(∀ (z,z'). id (h z) = h (id z))</code> which is true by similar reasoning.</li>
</ul>
<p>Hence, we may conclude that <code>BIMAP r t (bimap g h x) = bimap g' h' (BIMAP q s x)</code>, which is equivalent to</p>
<pre><code>BIMAP r id . bimap id h = bimap id h . BIMAP r id</code></pre>
<p>We can simplify this further using a few assumptions: first, we already showed that <code>BIMAP = bimap</code>. Second, since we are assuming (BFS), that is, <code>bimap f g = first f . second g</code>, setting <code>f</code> or <code>g</code> to <code>id</code> shows that <code>bimap f id = first f . second id = first f . id = first f</code> (by the <code>id</code> law for <code>second</code>), and likewise <code>bimap id g = second g</code>. Hence the equation becomes</p>
<pre><code>first r . second h = second h . first r</code></pre>
<p>And voila!</p>
<p>One can also show that the <code>id</code> laws plus parametricity together imply the composition laws—I will leave that as an exercise. (Hint: make appropriate choices for <code>g'</code>, <code>h'</code>, <code>q</code>, and <code>s</code>.)</p>

