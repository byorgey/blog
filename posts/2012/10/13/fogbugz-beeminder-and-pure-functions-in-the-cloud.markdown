---
title: FogBugz, Beeminder, and... pure functions in the cloud?
published: 2012-10-13T21:02:00Z
categories: meta
tags: API,Beeminder,cloud,FogBugz,functions,productivity,pure,web
---

For a number of years now, I've used a free personal instance of <a href="http://www.fogcreek.com/fogbugz/" title="FogBugz">FogBugz</a> to track everything I have to do.  At any given time I have somewhere between 50-150 open tickets representing things on my to-do list, and over the last four years I have processed around 4300 tickets.  This has been immensely successful at reducing my stress and ensuring that I don't forget about things.  However, it's been somewhat less successful at actually getting me to do stuff.  It's still all too easy to ignore the really important but intimidating tickets, or at times to simply ignore FogBugz altogether.

Just last week, I discovered <a href="https://www.beeminder.com/" title="Beeminder">Beeminder</a>.  I've only been using it a week, but early indications are that it just might turn out to be as revolutionary for my productivity as FogBugz was.  The basic idea is that it turns long-term goals into short-term consequences.  You set up arbitrary quantifiable goals, and Beeminder tracks your progress over time and takes your money if you get off track---but you get to set the amount, and in fact it's completely free until the <i>second</i> time you fail at a particular goal. In fact I haven't even pledged any money for any of my goals; just the threat of "losing" has been enough to motivate me so far. (In fact, I'm writing this blog post now because I made a <a href="https://www.beeminder.com/byorgey/goals/blogging">goal to write two blog posts a week</a>, and by golly, if I don't write a new post by tomorrow I'm going to LOSE!)

So, two great tastes that taste great together, right?  I could make Beeminder goal(s) to ensure that I close a certain number of tickets per week, or a certain number of high-priority tickets, or a certain number of tickets with a given tag, or whatever seems like it would be helpful.  Beeminder has a nice API for entering data, and FogBugz comes with a "URL trigger" plugin which can automatically create GET or POST requests to some URL upon certain events (such as closing a ticket matching certain criteria).  The URL trigger plugin lets you construct an arbitrary URL using a list of special variables which get filled in with values from the given ticket.  So I can just trigger a POST to the Beeminder URL for entering a data point, and give it arguments indicating the timestamp of the ticket event and a comment with the name of the ticket.  

No problem, right?

Well... almost.  There's just one tiny catch.  You see, FogBugz outputs timestamps in the format  YYYY-MM-DD HH:MM:SS... and Beeminder expects a number of seconds since the epoch.  Argggh!

I want to just plug in a little function in the middle to do the conversion.  But both the FogBugz and Beeminder APIs are running on remote servers that I have no direct control over.  I'd have to somehow send the FogBugz POST to some other server that I do control, munge the data, and forward it on to Beeminder.  But setting this up from scratch would be a lot of work, not to mention the expense of maintaining my own server.

Here's what I really want: a website where I can somehow write my function in a little domain-specific language, and get a URL where I can point FogBugz, which would cause my function to run on the timestamp and the result forwarded appropriately to Beeminder.  Of course there are issues to be worked out with security, DOS attacks, and so on, but it seems to me it should be possible in principle.

Does something like this already exist?  If not, why not?  (And how hard would it be to build one using all the great Haskell tools for web development out there? =)  It seems to me that the ability to write "glue" code like this to sit in between various APIs is becoming quite important.

