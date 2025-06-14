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
of numbers, and we should interpret the last digit of each number as an
exponent, then sum the results.  For example, if given `125`, we
should interpret it as $12^5$, and so on.

### Dealing with I/O via `interact`

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

### Solving the problem with a pipeline

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

Let's consider [Shopping List](https://open.kattis.com/problems/shoppinglist) as a second example.  In this
problem, we are given a list of shopping lists, where each shopping
list consists of a list of space-separated items on a single line.  We
are asked to find the items which are common to all the shopping
lists, and print them in alphabetical order.

### Wholemeal programming with standard data structures

This problem is very amenable to a ["wholemeal programming"
approach](https://www.cs.ox.ac.uk/ralf.hinze/publications/ICFP09.pdf),
where we work entirely at the level of whole data structure
transformations rather than looping over individual elements.  We can
turn each shopping list into a set, then find the intersection of all
the sets.  Moreover, if we use `Data.Set`, which uses an ordering on
the elements, we will get the result in alphabetical order "for free"
("free" as in the amount of code we have to write, not necessarily
runtime cost).  Haskell has a decent collection of data structures in
the `containers` library (`(Int)Set`, `(Int)Map`, `Seq`, `Tree`, and
even `Graph`) with a large collection of standard methods to construct
and manipulate them, which are bread and butter for many competitive
programming problems.

```haskell
{-# LANGUAGE ImportQualifiedPost #-}

import Control.Category ((>>>))
import Data.Set (Set)
import Data.Set qualified as S

main = interact $
  lines >>> drop 1 >>> map (words >>> S.fromList) >>>
  foldr1 S.intersection >>>
  (\s -> show (S.size s) : S.toList s) >>> unlines
```

### `ByteString` vs `String`

Unfortunately, when we try submitting this code, we get a Time Limit
Exceeded error!  What's wrong?

The issue is our use of `String`, which is an actual linked list of
characters and is very slow, especially when we have many short
strings, as in this problem.  In the worst case, we could have 100
shopping lists, each with 5000 items of length 10, for a total of up
to 5 MB of input; with that much input data to read, any overhead
associated with reading and parsing the input can make a significant
difference.

Switching to `ByteString` is much faster.  Why not `Text`, you ask?
Well, `Text` has to do a bunch of extra work to deal properly with
Unicode encodings, but in 99.99% of all competitive programming problems
I've ever seen, the input is guaranteed to be ASCII.  So not
only do we not need `Text`, we can get away with a version of
`ByteString` that simply assumes every character is a single 8-bit
byte!

Once we import it, all we need to do is replace a bunch of
`String` operations with corresponding `ByteString` ones.

```haskell
{-# LANGUAGE ImportQualifiedPost #-}

import Control.Category ((>>>))
import Data.Set (Set)
import Data.Set qualified as S
import Data.ByteString.Lazy.Char8 qualified as BS

main = BS.interact $
  BS.lines >>> drop 1 >>> map (BS.words >>> S.fromList) >>>
  foldr1 S.intersection >>>
  (\s -> BS.pack (show (S.size s)) : S.toList s) >>> BS.unlines
```

A Favourable Ending: input parsing and lazy recursive structures
---------------------------------------------------------------

As a last example, let's look at [A Favourable
Ending](https://open.kattis.com/problems/favourable).  This problem
consists of a number of test cases; each test case describes a
choose-your-own-adventure book with a number of sections, where each
section is either an ending (either good or bad), or allows the reader
to choose among three sections to proceed to next. For each test case,
we are asked how many distinct stories there are with good endings.

More abstractly, since we are guaranteed that there are no loops, the
sections of the book form a
[DAG](https://en.wikipedia.org/wiki/Directed_acyclic_graph), and we
are asked to count the number of distinct paths in a DAG from a
distinguished start node to any of a distinguished set of "good"
leaves.

### Parsing with Scanner

Parsing the input for this problem is trickier than the other
examples so far.  In theory, we could still ignore the first number
specifying the number of test cases, and just continue reading test
cases until EOF.  However, each test case begins with a number
specifying the number of sections in the book, and we cannot ignore
this number: we need to know how many lines to read before the start
of the next test case.  Doing this manually involves pattern-matching
on a list of lines, using `splitAt` to split off the lines for each
test case, and manually passing around the list of the remaining
lines: tedious.

Fortunately, Haskell is great at building abstractions to insulate us
from such tedium.  I've developed a [simple `Scanner`
abstraction](https://byorgey.github.io/blog/posts/2019/05/22/competitive-programming-in-haskell-scanner.html)
which works well in this context.

We begin by creating some data types to represent the input in
structured form:

```haskell
type Book = Map Int Section

data Section = End Disposition | Choice [Int]
  deriving (Eq, Show)

data Disposition = Favourably | Catastrophically
  deriving (Eq, Show, Read)
```

Now we can write a `Scanner` to read a `Book`:

```haskell
book :: Scanner Book
book = do
  s <- int
  M.fromList <$> s >< ((,) <$> int <*> section)

section :: Scanner Section
section = do
  t <- peek
  if isDigit (BS.head t)
    then Choice <$> (3 >< int)
    else End . readLower . BS.unpack <$> str

readLower :: Read a => String -> a
readLower = read . onHead toUpper

onHead :: (a -> a) -> [a] -> [a]
onHead _ [] = []
onHead f (x : xs) = f x : xs
```

(`readLower` and `onHead` are functions in my personal competitive
programming template, included here for completeness).

One more piece of boilerplate we can write at this point is the `main`
function, which simply consists of running the `Scanner` to read all the
test cases, solving each test case, and formatting the output.

```haskell
main = BS.interact $ runScanner (numberOf book) >>> map (solve >>> showB) >>> BS.unlines
```

### DP + topsort with a lazy recursive map

With all that framework out of the way, we can turn to actually
solving the problem.  And here is where something really fun happens.
In a typical imperative language, we would have to first topologically
sort the book sections, then use dynamic programming to compute the
number of good stories beginning at each section, starting with the
leaves and proceeding backwards through the topological sort to the
start---dozens of lines of code.  However, in Haskell we can get all
of this for free, just by defining a lazy, recursive map!

```haskell
solve :: Book -> Int
solve book = endings ! 1
  where
    endings = M.fromList [(p, endingsFrom (book!p)) | p <- M.keys book]
    endingsFrom (End d) = if d == Favourably then 1 else 0
    endingsFrom (Choice ps) = sum $ map (endings !) ps
```

`endings` is a `Map` from each book section to the number of favorable
stories starting with that section.  Notice how its values are defined
via the `endingsFrom` function, which is in turn defined, in the
`Choice` case, by looking up the values of the choices in the
`endings` map and summing them.  `endings` is thus defined
recursively, which works because it is lazy in the values.  When we
demand the value of `endings ! 1`, the runtime system starts evaluating
thunks in the map as needed, implicitly doing a topological sort for us.

Here's another way to think about this: what we really want is the
function `endingsFrom : Section -> Int`, which tells us how many good
endings there are starting at a given section. It can be defined via a
recurrence; however, if we were to literally implement it as a
recursive function, our program would spend a ridiculous amount of
time recomputing the same values over and over again.  So, we insert a
lazy map in the middle to memoize it (there are [other data
structures](https://byorgey.github.io/blog/posts/2023/06/06/dynamic-programming-in-haskell-automatic-memoization.html)
that can be used for this purpose as well).

Resources
---------

Here are some resources in case you're interested in exploring more.

- [Open Kattis](https://open.kattis.com) has a collection of thousands
  of high-quality problems which can be solved in Haskell (or many
  other languages).  If you just want to try solving some problems for
  fun, it's a great place to start.
- There are also other sites which accept Haskell, such as
  [Codeforces](https://codeforces.com/).  Check these out if you want
  to actually participate in timed contests.
- My public [listing of Kattis problems I have solved](http://ozark.hendrix.edu/~yorgey/kattis.html), with my own personal
  rating system.
- I've written a series of [blog posts](https://byorgey.github.io/blog/tag/competitive%20programming.html) about competitive
  programming in Haskell, on a variety of topics.
- I also have a [repository of modules](https://github.com/byorgey/comprog-hs/) I've developed
  specifically for competitive programming.  Many of the modules are
  documented in one or more blog posts.
- Soumik Sarkar has an even [larger collection of Haskell libraries for
  competitive programming](https://github.com/meooow25/haccepted).
