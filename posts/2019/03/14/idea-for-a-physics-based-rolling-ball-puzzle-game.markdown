---
title: Idea for a physics-based rolling ball puzzle game
published: 2019-03-14T10:19:53Z
categories: projects
tags: angular,ball,game,idea,physics,rolling,velocity
---

<p>For quite a while I’ve had this idea for a cool game, and had vague intentions to learn some game/physics framework well enough to make it, but I’ve finally admitted that this is never going to happen. Instead I’ll just describe my idea here in the hope that someone else will either make it, or tell me that it already exists!</p>
<p>This is a 2D physics-based puzzle/obstacle game where you control a ball (<em>aka</em> circle). The twist that distinguishes it from similar games I’ve seen is that you have only two ways to control the ball:</p>
<ul>
<li><p>Pushing the left or right arrow keys changes the ball’s <em>angular velocity</em>, that is, its rate of spin. If the ball is sitting on a surface, this will cause it to roll due to friction, but if the ball is in the air, it will just change its spin rate without changing its trajectory at all.</p></li>
<li><p>The down arrow key increases the ball’s velocity in the downwards direction. If the ball is sitting on a surface this will cause it to bounce upwards a bit. If the ball is in the air you can cause it to either bounce higher, by adding to its downward velocity while it is already falling, or you can dampen a bounce by pushing the down arrow while the ball is travelling upwards.</p></li>
</ul>
<p>Those are the key mechanics. My intuition is that controlling the ball would be challenging but doable, and there would be lots of opportunities for interesting obstacles to navigate. For example, to get out of a deep pit you have to keep bouncing higher and then once you’re high enough, you impart a bit of spin so the next time you bounce you travel sideways over the lip of the pit. Or there could be a ledge so that you have to bounce once or twice while travelling towards it to get high enough to clear it, but then immediately control your subsequent bounce so you don’t bounce too high and hit some sort of hazard on the ceiling. And so on.</p>
<p>Finally, of course there could be various power-ups (<em>e.g.</em> to make the ball faster, or sticky, or to alter gravity in various ways). Various puzzles might be based on figuring out which power-ups to use or how to use them to overcome various obstacles.</p>
<p>So, does this game already exist? Or does someone want to make it? (Preferably in Haskell? =)</p>

