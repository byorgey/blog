---
title: 'Collecting unstructured information with the monoid of partial knowledge'
published: 2008-04-17T21:32:00Z
categories: haskell,math
tags: knowledge,monoid,preorder
---

In <a href="http://byorgey.wordpress.com/2008/04/17/an-interesting-monoid/">my last post</a>, I described what I'm calling the "monoid of partial knowledge", a way of creating a monoid over sets of elements from a preorder, which is a generalization of the familiar monoid $(S,\max)$ over a set with a total order and a smallest element.

There's actually one situation where a special case of this monoid is commonly used in Haskell.  Suppose you have a record type which contains several fields, and you would like to parse some input to create a value of this type.  The problem is that the input is not very nice: the bits of input which designate values for various fields are not in any particular order; some occur more than once; some might even be missing.  How to deal with this?

One common solution is to wrap the type of each field in the record with Maybe, and create a Monoid instance which allows you to combine two partial records into one (hopefully less partial) record.  Using this framework, you can just parse each bit of input into a mostly-blank record, with one (or more) fields filled in, then combine all the records (with mconcat) into one summary record.  For example:

<pre>
import Data.Monoid
import Control.Monad  -- for the MonadPlus instance of Maybe

data Record = R { name  :: Maybe String,
                  age   :: Maybe Int }
  deriving (Eq, Show)

instance Monoid Record where
  mempty = R Nothing Nothing
  mappend r1 r2 = R { name = name r1 `mplus` name r2
                    , age  = age r1  `mplus` age r2
                    }
</pre>

The mplus function, from the MonadPlus instance for Maybe, simply takes the first Just value that it finds.  This is nice and simple, and works just great:

<pre>
&gt; mempty :: Record
R {name = Nothing, age = Nothing}
&gt; mconcat [mempty { name = Just "Brent" }]
R {name = Just "Brent", age = Nothing}
&gt; mconcat [mempty { name = Just "Brent" }, mempty { age = Just 26 }]
R {name = Just "Brent", age = Just 26}
&gt; mconcat [mempty { name = Just "Brent" }, mempty { age = Just 26 }, mempty { age = Just 23 }]
R {name = Just "Brent", age = Just 26}
</pre>

Note how the appending is left-biased, because mplus is left-biased: after seeing the first age, all subsequent ages are ignored.

Now, really what we're doing here is using the monoid (as in my previous post) induced by the preorder which says that any name is $\lesssim$ any other name, and any age is $\lesssim$ any other age, and names and ages can't be compared! 

Some code is in order.  First, we create a class for preorders, and a newtype to contain sets of elements (there's already a Monoid instance for Set, so we need a newtype to give a different semantics).  Then we translate the specification from my previous post directly into a Monoid instance.

<pre>
import qualified Data.Set as S

-- laws:
-- if x == y, then x &lt;~ y
-- if x &lt;~ y  and y &lt;~ z, then x &lt;~ z
class (Eq p) =&gt; Preorder p where
  (&lt;~) :: p -&gt; p -&gt; Bool

newtype PStar p = PStar { unPStar :: (S.Set p) }
  deriving Show

instance (Preorder p, Ord p) =&gt; Monoid (PStar p) where
  mempty      = PStar S.empty
  mappend (PStar ss) (PStar ts) = PStar $
    S.filter (\s -&gt; all (\t -&gt; s == t || t &lt;~ s || not (s &lt;~ t)) $ S.toList ts) ss
    `S.union`
    S.filter (\t -&gt; all (\s -&gt; s == t || not (t &lt;~ s)) $ S.toList ss) ts
</pre>

Pretty straightforward!  Of course, there are probably more efficient ways to do this, but I don't care about that at the moment: let's get some working proof-of-concept code, and worry about efficiency later.  Now, one thing you may notice is that our Monoid instance requires an Ord instance for p!  "But I thought we only needed a preorder?" I hear you cry.  Well, the Ord is just there so that we can use an efficient implementation of Set.  In particular, note that the Ord instance need have nothing at all to do with the Preorder instance, and won't affect the semantics of this Monoid instance in any way.  If we wanted, we could do away with the Ord entirely and use lists of unique elements (or something like that) instead of Sets.

So, how do we translate our record example from above into this new framework?  Easy, we just create a data type to represent the different kinds of facts we want to represent, along with a convenience method or two, and make it an instance of Preorder:

<pre>
data Fact = Name String | Age Int
  deriving (Eq,Show,Ord)

fact :: Fact -&gt; PStar Fact
fact f = PStar (S.singleton f)

instance Preorder Fact where
  (Name _) &lt;~ (Name _) = True
  (Age _)  &lt;~ (Age _)  = True
  _        &lt;~ _        = False
</pre>

Let's try it!

<pre>
&gt; mempty :: PStar Fact
PStar {unPStar = fromList []}
&gt; mconcat . map fact $ [Name "Brent"]
PStar {unPStar = fromList [Name "Brent"]}
&gt; mconcat . map fact $ [Name "Brent", Age 26]
PStar {unPStar = fromList [Name "Brent",Age 26]}
&gt; mconcat . map fact $ [Name "Brent", Age 26, Age 23]
PStar {unPStar = fromList [Name "Brent",Age 26]}
</pre>

Neato!  But the really cool thing is all the extra power we get now: we can easily tweak the semantics of the Monoid instance by altering the Preorder instance.  For example, suppose we want the first name that we see, but the <i>oldest</i> age.  Easy peasy:

<pre>
instance Preorder Fact where
  (Name _) &lt;~ (Name _) = True
  (Age m)  &lt;~ (Age n)  = m &lt;= n
  _        &lt;~ _        = False

&gt; mconcat . map fact $ [Age 23, Name "Brent"]
PStar {unPStar = fromList [Name "Brent",Age 23]}
&gt; mconcat . map fact $ [Age 23, Name "Brent", Age 24]
PStar {unPStar = fromList [Name "Brent",Age 24]}
&gt; mconcat . map fact $ [Age 23, Name "Brent", Age 24, Name "Joe", Age 26]
PStar {unPStar = fromList [Name "Brent",Age 26]}
</pre>

Of course, we could have done this with the Record model, but it wouldn't be terribly elegant.  But we're not done: let's say we want to remember the oldest age we find, and the first name, unless the age is over 50, in which case we don't want to remember the name (I admit this is a bit contrived)?  That's not too hard either:

<pre>
instance Preorder Fact where
  (Name _) &lt;~ (Name _) = True
  (Age m)  &lt;~ (Age n)  = m &lt;= n
  (Name _) &lt;~ (Age n)  = n &gt; 50
  _        &lt;~ _        = False

&gt; mconcat . map fact $ [Name "Brent", Age 26]
PStar {unPStar = fromList [Name "Brent",Age 26]}
&gt; mconcat . map fact $ [Name "Brent", Age 26, Age 45]
PStar {unPStar = fromList [Name "Brent",Age 45]}
&gt; mconcat . map fact $ [Name "Brent", Age 26, Age 45, Age 53]
PStar {unPStar = fromList [Age 53]}
</pre>

This would have been a huge pain to do with the Record model!  Now, this isn't an unqualified improvement; there are several things we can't do here.  One is if we want to be able to combine facts into larger compound facts: we can do that fairly straightforwardly with the Record-of-Maybes model, but not with the preorder-monoid model.  We also can't easily choose to have some fields be left-biased and some right-biased (the Monoid instance for PStar has left-bias built in!).  But it's certainly an interesting approach.

Now, one thing we do have to be careful of is that our Preorder instances really do define a preorder! For example, at first I tried using $n &lt; 18$ in the above Preorder instance instead of $n &gt; 50$, and was confused by the weird results I got.  But such a Preorder instance violates transitivity, so no wonder I was getting weird semantics. =)  It would be interesting to reformulate this in a dependently typed language like <a href="http://appserv.cs.chalmers.se/users/ulfn/wiki/agda.php">Agda</a>, where creating a Preorder could actually require a proof that it satisfied the preorder axioms.

Thanks to <a href="http://conal.net/">Conal Elliott</a> for some suggestions on making the formulation in the previous post more elegant -- we'll see what comes of it!

