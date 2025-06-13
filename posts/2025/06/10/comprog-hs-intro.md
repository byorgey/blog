---
title: 'Introduction to competitive programming in Haskell'
categories: competitive programming,haskell
tags: Kattis
katex: true
---

A few days ago I gave a talk at [ZuriHac
2025](https://zfoh.ch/zurihac2025/) entitled *Haskell for Competitive
Programming*, a basic introduction to competitive programming in
general, and the joy of using Haskell for competitive programming in
particular.  This is an expanded version of my talk in blog post form.
(For an even gentler introduction to competitive programming in
Haskell, see [this old blog post from
2019](https://byorgey.github.io/blog/posts/2019/04/24/competitive-programming-in-haskell-basic-setup.html).)

XXX generate blog "category" links!  Or at least generate a listing of
posts in the competitive programming category...

Competitive Programming
-----------------------

First of all, what is *competitive programming*?  It's a broad term,
but when I talk about competitive programming I have something in mind
along the following lines:

- There are well-specified input and output formats, usually with a
  few examples, and a precise specification of what the output should
  be for a given input.
- Your job is to write a program which transforms input meeting the
  specification into a correct output.
- You submit your program, which is tested on a number of inputs and
  declared correct if and only if it yields the correct output for all
  the tested inputs.
- There is often time pressure involved---that is, you have a limited
  amount of time in which to write your program. However, it is also
  possible to participate "recreationally", simply for the joy of
  problem-solving, without time pressure (in fact, the vast majority
  of the competitive programming I do is of this form, though I have
  occasionally participated in timed contests).

There are many variations: whether you are allowed to use code
libraries prepared ahead of time, or must type everything from
scratch; outputs can be scored according to some criteria rather
than simply being judged right or wrong; and so on.

There are many sites which allow you to participate in contests and/or
solve competitive programming problems recreationally.  My favorite is
[Open Kattis](https://open.kattis.com); I mention some others at the
end of this post.

Pot: a first example
--------------------

As an introductory example, let's look at
[Pot](https://open.kattis.com/problems/pot). As usual, there's a silly
story, but what it boils down to is that we will be given a sequence
of numbers; we should interpret the last digit of each number as an
exponent, and then sum the results.  For example, if given `125`, we
should interpret it as $12^5$, and so on.

An imperative approach to such a problem would involve doing a
sequence of input commands, some computation, and a sequence of output
commands---possibly interleaved with one another---and we might
immediately think to start using functions like `getLine` and
`putStrLn` to do the required I/O in Haskell.  However, there is a
much more fruitful functional perspective: we are simply being asked
to implement a particular (partial) function of type `String ->
String`.  The fact that the function's input and output should be
hooked up to the program's standard input and output is just an
implementation detail.  Competitive programming is functional at
heart!

It turns out that Haskell's standard library already has the perfect
built-in function for this scenario:
```haskell
interact :: (String -> String) -> IO ()
```
`interact` takes a pure `String -> String` function and turns it into
an `IO` action which reads from standard input, passes the input to
the given `String -> String` function, and prints the result to standard output.  It even
does this using *lazy* I/O---that is, the input is
read lazily, as demanded by the function, so that the output and input
can be automatically interleaved depending on which parts of the
output depend on which parts of the input. In particular, this means
that that the entire input need not be stored in memory at once. If
the inputs can be processed into outputs in a streaming fashion---as
is the case in the example problem we are currently
considering---then the input and output will be interleaved.  In
general, this kind of lazy I/O is
[problematic](https://stackoverflow.com/questions/5892653/whats-so-bad-about-lazy-i-o)
and even unsafe, but it's perfect for this scenario.

So `interact` does all the `IO` for us, and all we have to do is write
a pure `String -> String` function which transforms the input to the
output.  In this case, we can split the input into `lines`, `drop` the
first line (we don't need to know how many lines of input there
are---we just get a list of all of them, since `interact` will read
until EOF), `read` each number and turn it into the first digits
raised to the power of the last digit, then `sum` them and `show` the
result.  The full solution is below.  Notice how I use the "backwards
composition" operator `(>>>)`, since I find it more convenient to type
from left to right as I'm thinking about transforming from input to
output.

```haskell
import Control.Category ((>>>))

main = interact $
  lines >>> drop 1 >>> map (read >>> process) >>> sum >>> show

process :: Integer -> Integer
process n = (n `div` 10) ^ (n `mod` 10)
```

I use `Integer` here since raw performance doesn't matter much for
this easy problem, and `Integer` avoids any potential problems with
overflow.  However, using `Int` instead of `Integer` can make a big
difference for some compute-intensive problems.  On Kattis, `Int` will
always be 64 bits, but last time I checked `Int` can be 32 bits on
Codeforces.

Shopping List: wholemeal programming and ByteString
---------------------------------------------------

Let's consider [Shopping List](XXX) as a second example.  In this
problem, we are given a list of shopping lists, where each shopping
list consists of a list of space-separated items on a single line.  We
are asked to find the items which are common to all the shopping
lists, and print them in alphabetical order.

This problem is very amenable to a "wholemeal programming" approach:
XXX  We can turn each shopping list into a set, then find the
intersection of all the sets.

XXX use `Data.Set`.

XXX note `Set` vs `HashSet`?  `Set` is fast in practice; also gives us
sorted "for free".

```haskell
{-# LANGUAGE ImportQualifiedPost #-}

import Control.Category ((>>>))
import Data.Set (Set)
import Data.Set qualified as S

main = interact $
  lines >>> drop 1 >>> map (words >>> S.fromList) >>> foldr1 S.intersection >>> (\s -> show (S.size s) : S.toList s) >>> unlines
```

Unfortunately, when we try submitting this code, we get a Time Limit
Exceeded error!  What's wrong?

The issue is our use of `String`, which is an actual linked list of
characters and very slow, especially when we have a lot of short
strings, as in this problem.  In the worst case, we could have XXX
strings of length YYY, for a total input size of ZZZ.

Switching to `ByteString` is much faster.  Why not `Text`, you ask?
Well, `Text` has to do a bunch of extra work to deal properly with
Unicode encodings, but 99.99% of all competitive programming problems
I've ever seen guarantee that the input will be all ASCII.  So not
only do we not need `Text`, we can get away with a version of
`ByteString` that simply assumes every character is a single 8-bit
byte!

Once we import it, it's really just an issue of replacing a bunch of
string operations with corresponding `ByteString` ones.

```haskell
{-# LANGUAGE ImportQualifiedPost #-}

import Control.Category ((>>>))
import Data.Set (Set)
import Data.Set qualified as S
import Data.ByteString.Lazy.Char8 qualified as BS

main = BS.interact $
  BS.lines >>> drop 1 >>> map (BS.words >>> S.fromList) >>> foldr1 S.intersection >>> (\s -> BS.pack (show (S.size s)) : S.toList s) >>> BS.unlines
```

Favourable Endings: input parsing and lazy recursive structures
---------------------------------------------------------------

As a last example, let's look at [Favourable Endings](XXX).

XXX discuss Scanner, lazy recursive map (link to blog posts)

```haskell
main = BS.interact $ runScanner (numberOf book) >>> map (solve >>> showB) >>> BS.unlines

type Book = Map Int Section

data Section = End Disposition | Choice [Int]
  deriving (Eq, Show)

data Disposition = Favourably | Catastrophically
  deriving (Eq, Show, Read)

book :: Scanner Book
book = do
  s <- int
  M.fromList <$> s >< ((,) <$> int <*> section)

section :: Scanner Section
section = do
  t <- peek
  if isDigit (BS.head t)
    then Choice <$> (3 >< int)
    else End . read . onHead toUpper . BS.unpack <$> str

solve :: Book -> Int
solve book = favorableEndings ! 1
  where
    favorableEndings = M.fromList [(p, endings (book!p)) | p <- M.keys book]
    endings (End d) = if d == Favourably then 1 else 0
    endings (Choice ps) = sum $ map (favorableEndings !) ps
```

Resources
---------

- [Open Kattis](https://open.kattis.com) has a collection of thousands
  of high-quality problems which can be solved in Haskell (or many
  other languages).  If you just want to try solving some problems for
  fun it's a great place to start.
- There are also other sites which accept Haskell, such as XXX.  Check
  these out if you want to actually participate in timed contests.
- My public [listing of Kattis problems I have solved](XXX), with my own personal
  rating system.
- I've written a series of [blog posts](XXX) about competitive
  programming in Haskell, on a variety of topics.
- I also have a [repository of modules](XXX) I've developed
  specifically for competitive programming.  Many of the modules are
  documented in one or more blog posts.
- XXX has an even [larger collection of Haskell libraries for
  competitive programming](XXX).

