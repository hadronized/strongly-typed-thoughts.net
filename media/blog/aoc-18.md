On the 1st of December 2018, I decided to give it a try to [Advent of Code]. AoC is, basically, a
programming challenge website where you get two puzzles unlocked every day of December from 1st to
25th – hence the name. It has a ranking system which scores are based on the absolute time you took 
to solve the puzzles – i.e. spent time as soon as the puzzles got unlocked. As a French living in
Paris, I find this a bit unfair (we get puzzles unlocked at around 5:00 AM!) and then I just
realized I could do the puzzles for fun only.

This blog post sums up what I did with AoC#18, my thoughts about the puzzles and even a
meta-discussion about programming challenges. I used Haskell for most challenges and switched to
Rust for no specific reason. Just fun.

Also, because the puzzles’ texts are somewhat long, I will just give a link to the text instead of
copying it and will just make a very short description of the problem each time.

Finally, don’t expect to find all the puzzles: I stopped at day 15. I will explain the reasons why
at the end of this article. My adventure can be found
[here](https://github.com/phaazon/advent-of-code-2k18). Most puzzles have *input data*. You will
find them in day directories as `input.txt` files.

Enjoy the reading!

> **If you want to try and take the challenges, I advise you not to read any further as this article
> would spoil you solutions! You will need a Github account and… a lot of time.**

# --- Day 1: Chronal Calibration ---

[Text](https://adventofcode.com/2018/day/1)

## Part 1

> This puzzle is about summing arrays and finding duplicates (without counting).

First puzzle I discovered and first AoC challenge I ever took, I was surprised at the simplicity.
Basically, given lines containing a single number preceded by either `+` or `-`, we must compute
the sum of all the numbers.

The first thing to do is to parse the file into a stream of numbers. In Haskell, this was almost
a one liner:

```
main :: IO ()
main = do
    numbers <- fmap (map parse . lines) getContents
  where
    parse ('+':xs) = read xs -- (1)
    parse n = read n
```

If I removed the `+` from the file, I could even have gone with a one liner:

```
    numbers <- fmap (map read . lines) getContents
```

> The `read` function is not safe and I don’t recommend using it for real, production projects.
> In the context of this challenge, though, it was alright.

I don’t have much to say about this puzzle as I found it not very interesting.

## Part 2

We just want to get the first cumulative sum that appears twice. That is, each time a new number
is summed, we get a given “current sum”. We add positive and negative integers, so we might end up,
at some time, with the same number. Because we don’t know when that will happen, we need to turn
the input stream into an infinite stream of numbers that repeat over and over. Haskell has the
`cycle` function that does exactly that: it takes a list and makes it repeat infinitely.

What is great about `cycle` in Haskell is that it could be implemented in a very naive way and yet
yield good performance – thanks GHC!:

```
cycle :: [a] -> [a]
cycle x = let x' = x ++ x' in x'
```

Or, prefered (personal opinon):

```
cycle :: [a] -> [a]
cycle = fix . (++)
```

> `fix` is a very powerful function any Haskell should know. Typically, when you want to
> corecursively build something, it’s very likely you’ll need `fix`. I advise you to have a look at
> my [netwire tutorial](https://phaazon.net/blog/getting_into_netwire) in which I used `fix` pretty
> much everywhere.

There are several ways to fix this problem. I chose to use a **set** to hold all the known
intermediate sums. Because we’re working with integers, the Haskell `IntSet` type is perfect for
that. The algorithm is then just a special fold over an infinite list that stops when a sum is seen
twice by looking it up in the integer set:

```
findFreq _ freq [] = freq -- this shouldn’t ever happen since the list is infinite
findFreq knownFreq freq (change:xs)
    | newFreq `member` knownFreq = newFreq
    | otherwise = findFreq (insert newFreq knownFreq) newFreq xs
  where
    newFreq = freq + change
```

[Haskell solution](https://github.com/phaazon/advent-of-code-2k18/blob/master/day-01/src/Main.hs)

# --- Day 2: Inventory Management System ---

[Text](https://adventofcode.com/2018/day/2)

## Part 1

This puzzle requires you to recognize and count characters in strings. For each string, you must
check whether they have any letter that appears exactly once and any letter that appears exactly
three times. Adding those counts provides a checksum for a list of strings.

I did that in Haskell as well. My idea was to use a count-array of size 26 (since the strings are
only in `[a-z]`. So the first part of my algorithm is to count, for a given string, the number of
time each letter occurs. This is done really easily with a fold that increments the proper value
value in the array at the index representing the character folded on in the string. That index
is obtained by turning a character into a numerical representation and subtracting it the numerical
value of `'a'`:

```
treatHash :: (Natural, Natural) -> String -> (Natural, Natural)
treatHash occurs hash =
    let result :: State = foldl' replace initialState hash
    in addPair occurs (foldl' incrOccur (0, 0) result)
  where
    replace st l = accum (+) st [(ord l - ord 'a', 1)]
    incrOccur i@(twos, threes) x
      | x == 2 = (1, threes)
      | x == 3 = (twos, 1)
      | otherwise = i
addPair (a, b) (c, d) = (a + c, b + d)
```

The `replace` function performs the update in an input vector (point-free function). This will
effectively increment the vector’s value at index `3` if the character is `d` (because
`ord 'd' - ord 'a' = 3`. `incrOccur`, here, is used to fold over the resulting array (`Vector`) and
compute if a letter appears twice and if a letter appears three times. You will notice that this
function doesn’t sum. For instance, if you have `aabb`, you get no letter that appears three times
but you have two that appear exactly twice. However, `incrOccur` will give you `(1, 0)`, because the
puzzle states it should be done this way. Finally, `addPair` add the number of twos and threes to a
pre-defined input – that will be handy for an outer fold.

The algorithm is then run on the input and we get the final result:

```
hashes <- fmap lines getContents
let checksum = uncurry (*) $ foldl' treatHash (0, 0) hashes 
```

We transform the standard input into lines – i.e. `[String]` – and then fold them with our
`treatHash` function. Finally, `uncurry (*)` is a little oneliner to simply multiply the left part
of a pair with its right part.

## Part 2

This part is about finding strings that are almost the same and differ and only by one letter. As 
doing a programming challenge, the first thing I want to find is *a solution*, not the best
solution. I also think people should really learn from this:

  1. Make it work.
  2. Make it clean.
  3. Make it fast.

Especially on programming challenges, you’ll be very, very unlikely to implement anything more than
(1.), because the inputs are often two small to really benefit from a real optimization. Your time
is a real important resource, don’t waste it. Measure whether you really need an optimization. I’m
not saying that you should be doing something bad because it’s easier. I’m saying that if it takes
you N minutes to write a solution that runs in M milliseconds, if you know that you could do it in,
for instance, 10N minutes to write a solution that runs in only 0,7M milliseconds for the foreseen
milliseconds, will, you’re going to waste your time.

So, in my case, I started with a naive approach that runs in *O(N²)*: comparing all strings to all
others:

```
searchCommon :: [String] -> String
searchCommon [] = "" -- the empty list has nothing in common
searchCommon (h:hs) = search h hs -- for each string, search in all remaining
  where
    search h [] = searchCommon hs -- exhausted,  search with the next string
    search h (x:xs)
      | almostSame h x = commonLetters h x -- grab the common letters if almost the same
      | otherwise = search h xs -- if not, just try the next string in the sub-list
```

The algorithm is pretty straight-forward. The `searchCommon` and `search` functions are mutually
recursive functions that go from, respectively, the whole list of strings to test and the local
tail as we advance. `almostSame` is defined as follows:

```
almostSame :: String -> String -> Bool
almostSame = go 0
  where
    go diffNb (l:ls) (r:rs)
      | diffNb > 1 = False
      | l /= r = go (succ diffNb) ls rs
      | otherwise = go diffNb ls rs
    go diffNb _ _ = diffNb == 1
```

This function is a special `zip` that short-circuits if it knows there are two many differences.
When both the input strings are exhausted, if `diffNb == 1`, then only one character has changed, so
the the overwhole function evaluates to `True`.

The final part we need is `commonLetters`, that is pretty straight-forward, too:

```
commonLetters :: String -> String -> String
commonLetters (l:ls) (r:rs)
  | l == r = l : commonLetters ls rs
  | otherwise = ls -- all the rest is identic, smartass
```

We construct a list that has the same characters as the input lists as long as they’re equal. As
soon as they differ, we discard the character and just return any of the input lists – they’re
equal. This function only works, obviously, if both the input strings are almost identical (only one
character differs and they have the same length). The `otherwise` branch is a short-circuit
optimization that prevents us from traversing the whole inputs after the difference.

[Advent of Code]: https://adventofcode.com/about
[referential transparency]: https://wiki.haskell.org/Referential_transparency
