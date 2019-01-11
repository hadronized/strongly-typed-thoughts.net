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

<!-- vim-markdown-toc GFM -->

* [Day 1: Chronal Calibration](#day-1-chronal-calibration)
    * [Part 1](#part-1)
    * [Part 2](#part-2)
* [Day 2: Inventory Management System](#day-2-inventory-management-system)
    * [Part 1](#part-1-1)
    * [Part 2](#part-2-1)
* [Day 3: No Matter How You Slice It](#day-3-no-matter-how-you-slice-it)
    * [Part 1](#part-1-2)
    * [Part 2](#part-2-2)
* [Day 4: Repose Record](#day-4-repose-record)
    * [Part 1](#part-1-3)
    * [Part 2](#part-2-3)
* [Day 5: Alchemical Reduction](#day-5-alchemical-reduction)
    * [Part 1](#part-1-4)
    * [Part 2](#part-2-4)
* [Day 6: Chronal Coordinates](#day-6-chronal-coordinates)
    * [Part 1](#part-1-5)
    * [Part 2](#part-2-5)
* [Day 7: The Sum of Its Parts](#day-7-the-sum-of-its-parts)
    * [Part 1](#part-1-6)
    * [Part 2](#part-2-6)
* [Day 8: Memory Maneuver](#day-8-memory-maneuver)
    * [Part 1](#part-1-7)
    * [Part 2](#part-2-7)
* [Day 9: Marble Mania](#day-9-marble-mania)
    * [Part 1 & 2](#part-1--2)
* [Day 10: The Stars Align](#day-10-the-stars-align)
    * [Part 1](#part-1-8)
    * [Part 2](#part-2-8)
* [Day 11: Chronal Charge](#day-11-chronal-charge)
    * [Part 1](#part-1-9)
    * [Part 2](#part-2-9)
* [Day 12: Subterranean Sustainability](#day-12-subterranean-sustainability)
    * [Part 1](#part-1-10)
    * [Part 2](#part-2-10)
* [Day 13: Mine Cart Madness](#day-13-mine-cart-madness)
    * [Part 1](#part-1-11)
    * [Part 2](#part-2-11)
* [Day 14: Chocolate Charts](#day-14-chocolate-charts)
    * [Part 1](#part-1-12)
    * [Part 2](#part-2-12)
* [Day 15: Beverage Bandits](#day-15-beverage-bandits)
* [Part 1](#part-1-13)
* [Conclusion](#conclusion)

<!-- vim-markdown-toc -->

# Day 1: Chronal Calibration

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
> my [netwire tutorial] in which I used `fix` pretty much everywhere.

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

# Day 2: Inventory Management System

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

[Haskell solution](https://github.com/phaazon/advent-of-code-2k18/blob/master/day-02/src/Main.hs)

# Day 3: No Matter How You Slice It

[Text](https://adventofcode.com/2018/day/3)

## Part 1

This problem was the first “challenging” as it required more thinking. The idea is that a very large
area – the text gives us the hint it is at least `1000` inches on each side but truly, we do not
need this information – must be sliced into several rectangular smaller areas (with edges aligned
with X and Y axis). Obviously, some rectangular area might overlap and the aim of the first part of
this problem is to find out **the total overlapping area**.

My idea was simple: we can trick and change the problem by discretizing it. This is something I
have said already but most of AoC problems have *hidden properties* and *hidden hints*. By thinking
more about the form of the solution, the area to find is expressed in *inches²*. The rectangular
areas are all lying on a 2D grid (1000×1000 at least, remember?) and their coordinates are always
natural numbers (they belong to `[0;+∞]`). Then, my idea was to state that 1 inch² is actually a
single “cell” at any coordinate in that grid – imagine that as a pixel. 4 inches² would be a 2×2
region in that grad (yielding 4 cells).

So instead of using the general idea of an area (M×N for a rectangle which sides are M and N long
wide), we can simply break a rectangular area into its most atomic components (1×1 cells)… and sum
them up! We will effectively end up with the area of this area.

A more interesting property of my way of solving it: we can now have a big array mapping to each
1×1 cell the number of times a rectangle lies on it. When iterating through all the rectangles, we
just break them into a list of 1×1 cells, look ingg up and updating the big count array. Once we’re
done, we just have to filter that big array to remove any element which count is less or equal to 1.
The remaining elements are 1×1 cells that contain at least two overlapping rectangles – we don’t
really care about the number. We don’t actually care about those elements: the length of that
filtered array is the area we are looking for, because it is the sum of 1×1 elements!

Converting a rectangular area – called claim in the text – into 1×1 cells is very easy in Haskell:

```
type Plan = Map (Int, Int) Natural -- the big array I told you
type Claim = (Id, Pos, Area)
type Id = Text -- the “name” of the fabric the rectangular area is about
type Pos = (Int, Int)
type Area = (Int, Int)

claim1x1 :: Claim -> [Pos]
claim1x1 (_, (line, row), (width, height)) =
  [(x, y) | x <- [line .. line + width - 1], y <- [row .. row + height - 1]]
```

As you can see, I chose to use a `Map (Int, Int) Natural` instead of an array to store the number
of overlaps per 1×1 cell. That enables us not to care at all about the size of the grid (remember
when I told you we don’t need the `1000` hint?).

The function that updates the `Plan` to take a claim into account is:

```
checkClaims :: [Claim] -> Plan
checkClaims = foldl' (\p (x, y) -> checkInch x y p) mempty . concatMap claim1x1

checkInch :: Int
          -> Int
          -> Plan
          -> Plan
checkInch line row = insertWith (+) (line, row) 1
```

Given a list of claims (`[Claim]`), `checkClaims` maps the `claim1x1` function and concats the
result, yielding a `[Pos]` list. That list is then folded over with the `checkInch x y p` function,
that takes an empty map as initial value. `checkInch` just increment the value found in the map if
it already exists; otherwise, it sets that value to `1`.

Finally, we need to compute the area:

```
overlappingInches :: Plan -> Int
overlappingInches = length . M.filter (> 1)
```

As I told you, that is crystal clear: it’s just the length of the filtered map.

## Part 2

This part is interesting also: you need to find out the `Id` of the claim that doesn’t overlap with
any other claim. I will not go into too much details about the algorithm as it’s very similar to the
previous one: instead of storing the number of overlaps by 1×1 cell, we store a `Set Id`, giving all
claims that are overlapping – we can see that as a more general form of the first part. We also need
a `Map Id Natural` that maps a fabric and the number of times it overlaps another. The fabric that
doesn’t overlap any other is then easily identifiable within that map: it has its associated value
set to `0`:

```
searchNonOverlapped :: [Claim] -> Maybe Id
searchNonOverlapped claims =
    case M.toList filtered of
      [(i, _)] -> Just i -- the text supposes there’s only one
      _ -> Nothing
  where
    (_, overlapped) = indexClaims claims
    filtered = M.filter (== 0) overlapped -- hello
```

[Haskell solution](https://github.com/phaazon/advent-of-code-2k18/blob/master/day-03/src/Main.hs)

# Day 4: Repose Record

[Text](https://adventofcode.com/2018/day/4)

## Part 1

Aaaah, day 4… I really don’t get why I got so annoyed by this one. It is quite simple in theory. But
that made me realize something I don’t really like about AoC: the *hidden rules*. You will see, if
you read this whole article, that some puzzles require you to make very important assumptions about
the input – and there’re a lot of assumptions you could make, so you have to make the right ones!

This puzzle is about guards that must protect a prototype manufacturing lab. You are given an
unordered list of events that occur about the guards:

  - When they begin their shifts (you are given the ID of the guard here).
  - When they fall asleep.
  - When they wake up.
  - Each action gives you a timestamp so that you can re-create the historical events.

The goal of the part 1 is to find the guard that has the most minutes asleep, and especially, find
the minute it is asleep the most – we must multiply the ID of the guard by the minute.

Obviously, the first part of this puzzle is to re-order the input to have it chronologically. Here
is the setup code:

```
data Entry = Entry {
    timestamp :: LocalTime,
    action :: Text
  } deriving (Show)

type GuardianId = Natural

data Action
  = BeginShift GuardianId
  | WakeUp
  | FallAsleep
    deriving (Eq, Ord, Show)

entryFromString :: Text -> Maybe Entry
entryFromString s = case split (== ']') s of
    [timestamp, action] -> Just $ Entry (parse . unpack $ T.drop 1 timestamp) action
    _ -> Nothing
  where
    parse = parseTimeOrError False defaultTimeLocale "%Y-%-m-%-d %H:%M"
```

The parsing part is not really interesting as it’s just challenge code: nasty but working parsing
code. :D

I then re-ordered the input with:

```
entries <- fmap (fromJust . traverse entryFromString . T.lines) T.getContents
let byTimeSorted = sortBy (comparing timestamp) entries
```

By the way, that code made me want to [tweet about how Haskell is actually pretty easy to read and
reason about](https://twitter.com/phaazon_/status/1070054255887822848). Anyway.

The next part of the algorithm is to transform the entries into a list of timed action. I actually
decided to stream it so that I could benefit from Haskell’s stream fusion – and because it’s so
simple and transparent:

```
readGuardianId :: Text -> Natural
readGuardianId = readT . T.drop 1

treatEntries :: [Entry] -> [(LocalTime, Action)]
treatEntries = map $ \entry ->
  let time = timestamp entry
  in case T.words (action entry) of
    ["Guard", ident, "begins", "shift"] -> (time, BeginShift $ readGuardianId ident)
    ("falls":_) -> (time, FallAsleep)
    ("wakes":_) -> (time, WakeUp)
    _ -> error "lol"
```

That is like mixing streaming and parsing at the same time. Then, the core of my algorithm: dispatch
the actions by guard. That is mandatory if we want to actually accumulate the “state” of a guardian
(when they’re sleeping, waking up, etc.). Otherwise, we get interleaved results.

```
dispatchActions :: [(LocalTime, Action)] -> Map GuardianId [(LocalTime, Action)]
dispatchActions = go mempty Nothing
  where
    go guardians _ ((t, action@(BeginShift gid)):xs) =
      go (insertAction gid t action guardians) (Just gid) xs

    go guardians jgid@(Just gid) ((t, action):xs) = go (insertAction gid t action guardians) jgid xs

    go guardians _ [] = fmap V.toList guardians

    go _ _ _ = error "dispatchActions: the impossible fucking occurred!"

    insertAction gid t action guardians =
      M.insertWith (flip (<>)) gid (V.singleton (t, action)) guardians
```

This is a by-hand fold that just applies the rule of beginning a shift (storing the ID of the
guardian that went napping so that we can correctly dispatch the remaining events).

Then the tricky part:

```
type Minute = Natural
type Minutes = [Minute]

minutesCounts :: [(LocalTime, Action)] -> Minutes
minutesCounts = go zeroMinutes Nothing
  where
    zeroMinutes = replicate 60 0 -- (1)
    asMinutes = todMin . localTimeOfDay

    -- the guard was sleeping
    go minutes (Just sleepTime) ((t, action):xs) =
      case action of
        BeginShift _ -> go minutes Nothing xs
        FallAsleep -> go minutes (Just t) xs -- not sure if that would even occur in the input
        WakeUp -> go (addSleepCount minutes (asMinutes sleepTime) (asMinutes t)) Nothing xs

    -- the guard was awake, so we’re only interested in when they go to sleep
    go minutes Nothing ((t, action):xs) =
      case action of
        FallAsleep -> go minutes (Just t) xs
        _ -> go minutes Nothing xs

    go minutes _ [] = minutes

    addSleepCount minutes sleepTime t = zipWith (+) minutes range -- (2)
      where
        -- this function is a bit hacky but it generates, for a given range of time, a list of 60
        -- elements where the time period has 1 and all the other has 0 (I leave you to the
        -- exercise of making that a better function)
        range :: Minutes
        range = replicate sleepTime 0 <> replicate (fromIntegral t - sleepTime) 1 <> replicate (60 - t) 0
```

This big function generates a list which length is 60 – mapping the number of times a guard has
passed sleeping at a given minute from midnight to 1:00 AM (see `(1)` and `(2)`).

Finally, what we need is a way to compute frequencies – or counts. That is, given a list of anyting,
compute that number of time a given anything happens in the list. I wrote a small utility function
for that – I got inspired by [\@jle], thanks!:

```
freqTable :: (Ord a) => [a] -> Map a Count
freqTable = M.fromListWith (+) . map (,1)
```

Then, finding the guard that has slept the more and the minute is easy:

```
findMostOccurring :: Map a Count -> (a, Count)
findMostOccurring = maximumBy (comparing snd) . M.toList -- and Haskell is hard?! ;)

findSleepiest :: Map GuardianId [(LocalTime, Action)] -> (GuardianId, (Minute, Count))
findSleepiest =
    fmap (findMostOccurring . freqTable . spanIndex) . maximumBy (comparing $ sum . snd) . M.toList . fmap minutesCounts
  where
    spanIndex = concatMap (\(i, x) -> replicate (fromIntegral x) i) . zip [0..]
```

We first find the guard that has the most time asleep (`maximumBy (comparing $ sum . snd)`. Then,
we find the minutes at which they were asleep the most (`findMostOccurring`). We are given the guard
ID, the given minute and the number of times they were asleep at that minute. Yay!

## Part 2

For this part, we would like to know which guard is most frequently asleep on the same minute? We
already have written all the code needed for that:

```
findMostFrequentlySleepy :: Map GuardianId [(LocalTime, Action)] -> (GuardianId, Minute)
findMostFrequentlySleepy =
    fmap findMin . maximumBy (comparing $ maximum . snd) . M.toList . fmap minutesCounts
  where
    findMin = fst . maximumBy (comparing snd) . zip [0..]
```

Instead of summing, we find the maximum time a guard was asleep. Pretty easy.

[Haskell solution](https://github.com/phaazon/advent-of-code-2k18/blob/master/day-04/src/Main.hs)

# Day 5: Alchemical Reduction

[Text](https://adventofcode.com/2018/day/5)

## Part 1

That puzzle is very natural to solve in Haskell. You are given an ASCII string that contains only
letters (lower case and upper case) that represent polymers. You must compute their final reduction
by following some basic rules:

  - Two adjacent letters will cancel them out if they don’t have the same case (lower vs. upper) but
    are the same letter. For instance, `aA` and `Bb` cancel each other but not `aa` nor `BB`.
  - This rule happens until no more reduction is possible. For instance, `AbBa` will first get
    reduced to `Aa` because `aB` will cancel out, but then `Aa` will also cancel out and we will be
    left with nothing.

You must give the number of units left in the final reducted polymer after all reductions have
occurred.

As I said, that is very simple and elegant in Haskell:

```
reduce :: String -> String
reduce = go []
  where
    go [] (x:xs) = go [x] xs
    go a [] = a
    go (a:xs) (b:bs)
      | not (isLetter b) = go (a:xs) bs
      | (toLower a /= toLower b) || (isLower a && isLower b) || (isUpper a && isUpper b) = go (b:a:xs) bs
      | otherwise = go xs bs
```

I decided to use a [zipper]-like traversal. My idea is the following:

  - Read a character. If we have nothing previously seen, just place it in the previous list.
  - If we have something previously seen, check whether it reacts with the last previously seen
    character. If so, just drop it and drop the previously seen character, and go on with the next
    character.
  - When we have exhausted the input list, the previously seen list of characters is the final form
    of the reduction.

This algorithm allows me to reduce by doing a forwards-and-backwards kind of sweeping, yielding
nice performance. Also, notice that the resulting list is reversed because of how we accumulate the
seen characters. Because we don’t care about the order, we will not reverse it back to its original
order.

The result is just the length of the output list.

## Part 2

This part asks us to find the polymer that is the smaller if we remove one kind of unit (a single
letter type). So if we remove `a` for instance, we must remove all `a` and `A`.

As there’re only 26 possible solutions (from `a` to `z`), and because my solution to part 1 was
already fast, I decided to go brute-force with this one: reducing the input string without a’s,
reducing the input string without b’s, reducing without c’s, etc. And then simply take the shorter
one.

```
bruteForce :: String -> Int
bruteForce polymers = minimum (map length allReduced)
  where
    types = ['a' .. 'z']
    allReduced = map (\c -> reduce $ filter (\x -> toLower x /= c) polymers) types
```

Winner winner chicken dinner.

[Haskell solution](https://github.com/phaazon/advent-of-code-2k18/blob/master/day-05/src/Main.hs)

# Day 6: Chronal Coordinates

[Text](https://adventofcode.com/2018/day/6)

## Part 1

That puzzle was one of the funniest I did. The idea is that, given an infinite 2D map, you are given
a list of several *points of interest* (POIs) in the form of `(x, y)` coordinates. The goal, for
this first part, is to find the largest zone in which all points have the same POI. What it means
is that, given several POIs, every positions on the map has a nearest POI (it can have several if
it’s at equal distance to several POIs – those must be discarded by the algorithm so that they do
not count into any zone). Several positions with the same nearest POI and adjacent to each others
form a zone, so that anyone in that zone knows that the nearest POI is the same accross all spanning
positions of the zone – you can picture the zone easily as discs centered on the POIs, but deformed
by other POIs.

The tricky part is that the map is infinite and POIs are scattered around it.

My idea was based on something I do **a lot** on my spare time with my 3D projects: compute AABBs.
An [AABB] is an enclosing box in which all points lie. The idea is that its size must be as minimal
as possible. An [AABB], which stands for Axis-Aligned Bounding Box, is the minimal bounding volume
that enclose a set of points and which has its edged aligned with the axis (in our case, the X and
Y axis). By the way, an [AABB] in 2D is also called **MBR**, which stands for Minimum Bounding
Rectangle. I chose AABB instead of MBR because I’m more used to work in 3D, but they’re essentially
the same thing.

So, the first thing I wanted to do is to compute the AABB of all the POIs for a two reasons:

  - The most important one: it helped me reduce the problem space from an infinite space to a
    finite one: instead of having an infinite 2D map, I now have a finite rectangular 2D region to
    explore. Which is, you’ll have to admit, much more comfortable. ;)
  - If you read the puzzle’s text, you’ll have notice this:
    - *“Your goal is to find the size of the largest area that isn't infinite.”*
    - What it means is that all the points **outside** of the AABB are in an area that is always
      infinite, so we will never be interested in those zones.

Ok, let’s see my code:

```
type Point = (Int, Int)

-- Note to readers: this the way I like to encode AABB because it eases some operations on points.
-- The lower point is a point that belongs to the AABB that satisfies the rule that no other point
-- with different coordinate are lower than it. Same thing for upper. I also like that encoding
-- because generating an AABB from a set of points is trivial.
data AABB = AABB {
    aabbLower :: Point,
    aabbUpper :: Point
  } deriving (Eq, Show)

findAABB :: [Point] -> AABB
findAABB [] = error "nein" -- this will never be called, so we don’t care about type safety here
findAABB (a:ls) = foldl' updateAABB (AABB a a) ls
  where
    updateAABB (AABB (lx, ly) (ux, uy)) (x, y) = AABB {
        aabbLower = (min (min lx x) lx, min (min ly y) ly),
        aabbUpper = (max (max ux x) ux, max (max uy y) uy)
      }

-- This function gives me a list of points that are in the AABB. It actually gives me all the points
-- the AABB wraps.
aabbToStream :: AABB -> [Point]
aabbToStream (AABB (lx, ly) (ux, uy)) = [(x, y) | x <- [lx .. ux], y <- [ly .. uy]]

-- Test whether a point lies on any edges of the AABB. You’ll get why this function is important
-- later.
liesOnAABB :: Point -> AABB -> Bool
liesOnAABB (x, y) (AABB (lx, ly) (ux, uy)) = x == lx || x == ux || y == ly || y == uy
```

I annotated the code with comments so that you can get what it’s for.

So, I create the `AABB` and then I call `aabbToStream` in order to get all points. You might already
have guessed the next step: we are going to find the nearest POI to all the points. For this, I
just went naive and just computed the [Manhattan distance] to all POI and kept the smallest. If we
map that function to all coordinates generated by the AABB, we get the first part of the solution.

```
manhDist :: Point -> Point -> Int
manhDist (a, b) (c, d) = abs (a - c) + abs (b - d)

nearest :: Point -> [(Int, Point)] -> Maybe Int
nearest p points =
  case sortBy (comparing snd) $ map (\(i, x) -> (i, manhDist p x)) points of
    [a] -> Just (fst a)
    a:b:_ -> if snd a == snd b then Nothing else Just (fst a) -- (1)
    _ -> error "nearest"
```

Here, `(1)` applies the rule I described earlier about at least two POIs at the same distance: we
just discard the point and it doesn’t participate in creating any zone.

Then, how do we find the biggest area? Easy: we re-use our `freqTable` function from Day 4 to
compute a frequency table! In my case, I just renamed that function `freqs`:

```
freqs :: (Ord a) => [a] -> Map a Natural
freqs = fromListWith (+) . map (,1)
```

If we call that function on a list of `[Int]`, we end up with `Map (Maybe Int) Natural` that
gives us the number of positions a given POI is the nearest. It’s perfect, because it’s exactly
what we are looking for!

```
biggestArea :: [Maybe Int] -> Natural
biggestArea = snd . maximumBy (comparing snd) . M.toList . freqs . catMaybes
```

Here, `catMaybes` just remove the `Nothing` case so that we go from `[Maybe Int]` to `[Int]`. We
then find out which POI has the biggest number of nearest positions and we simply return it. Because
those are positions, their sum is then the area of the zone: we’re done. Or almost. Remember that
some zones have an infinite areas. Thanks to the AABB, it’s actually easy to find which ones: those
have at least one point that lies on the AABB’s edges. We just have to iterate through all the
points and black list some points:

```
blackListPoints :: [(Point, Maybe Int)] -> AABB -> Set Int
blackListPoints points aabb = foldl' blacklist mempty points
  where
    blacklist blist (p, Just i) = if liesOnAABB p aabb then S.insert i blist else blist
blacklist blist _ = blist
```

## Part 2

The part 2 asks something different: now we want to find the area of the region containing all
locations for which the total distance to all POI is less than a given constant (`10000`). My
solution was actually way easier than expected, surprisingly:

```
safeArea = filter (\p -> sum (map (manhDist p) coords) <= 10000) points
```

Done. :)

[Haskell solution](https://github.com/phaazon/advent-of-code-2k18/blob/master/day-06/src/Main.hs)

# Day 7: The Sum of Its Parts

[Text](https://adventofcode.com/2018/day/7)

## Part 1

Here we go again: graph theory. Fortunately for us, it’s not a hard graph puzzle. That first part is
to simply display a string that shows the order in which a graph must be traversed. If two nodes can
be traversed at the same time, the node which letter comes first alphabetically is traversed first.

I’ll just show the traversal because the rest is not really interesting for that puzzle:

```
-- The graph encodes the relationship in reverse: it maps each node its list of dependencies.
-- So if we have something like A -> [], it means that the A node doesn’t have any dependency.
type Graph = Map Step (Set Step)
type Step = Char

-- Get the list of all available steps; i.e. they don’t have any dependency.
getAvailable :: Graph -> [Step]
getAvailable gr = [step | (step, set) <- M.toList gr, S.null set]

-- Traverse the graph and get the ordered steps to go through.
stepAvailable :: Graph -> [Step]
stepAvailable gr = case sort (getAvailable gr) of
  [] -> []
  (s:sx) -> s : stepAvailable (removeStep s gr)

removeStep :: Step -> Graph -> Graph
removeStep s = purgeDep s . M.delete s
  where
    purgeDep = fmap . S.delete
```

It’s a typical functional problem that gets solved very easily in Haskell.

## Part 2

The second part is pretty interesting. Instead of stepping through all the steps sequentially, you
ar given a *pool of workers*. It will take a given amount of time for a given worker to complete a
task and able us to visit a given node in the graph. We have to guess how many time it will take to
complete all of the steps.

I won’t post the code (it’s on GitHub if you want to have a look at it) as it’s a bit boring and the
idea of my solution is enough. The concept is to have a stepped simulation (i.e. you perform a set
of action in a given *“round”*, then repeat). In my case, each round is composed of several steps:

  1. First, partition the current work load into a set of *done tasks* and *running tasks*. This is
     quite easy to do by just checking at the remaining time of each task. If the remaining time is
     `0`, then it’s done, otherwise it’s still running.
  2. Generate the *time increment*. This is the minimal duration until a next interesting action
     occurs (i.e. a task gets done). Nothing can happen below that duration. That value can easily
     be found by looking up the remaining duration of the running tasks and taking the minimum.
  3. If we still have running tasks, step forward (i.e. recursively call the same function) by
     advancing the current time by the time increment and removing the done tasks.
  4. The *backlog*, that is implicit, can be created by monoidal operations and is detailed in the
     code on GitHub.
  5. When the backlog gets empty, we have the final time and the answer to the initial question.

This part was interesting because it made me write a parallel graph traversal (a graph task
scheduler) that could be used as base for a real and parallelized (I/O) task scheduler. Interesting
stuff.

[Haskell solution](https://github.com/phaazon/advent-of-code-2k18/blob/master/day-07/src/Main.hs)

# Day 8: Memory Maneuver

[Text](https://adventofcode.com/2018/day/8)

## Part 1

In its simple form, this puzzle is not really interesting and I could definitely present you the
solution in a single paragraph. However, I found it pretty fun to do so I’ll go a bit further.

The problem is the following: we are given a list of numbers that represent a data structure. That
data structure is basically a tree with tagged metadata. The goal is to parse the list of numbers
to generate a memory representation of that tree and compute checksums on it. The structure is:

  - A header, that has the number of children of a given node and the number of metadata entries.
  - Zero or many children.
  - At least one or many metadata entries.

The file format is made so that the data are nested. I’ll copy and explain an example:

```
2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
A----------------------------------
    B----------- C-----------
                     D-----
```

Here, only the first line is present in the input file. The first `2` means that the first (`A`)
node has two children (we don’t know anything about them yet) and the `3` means it has three
metadata. Those are the header. Then, since it has two children, the next `0` is the start of the
header of its first children (`B`), which has no child and three metadata (`3`). The next `10`, `11`
and `12` are then those metadata (since it doesn’t have any child). This node is then complete. If
you go back up in the tree, you know that `A` still has another child. So the next number, `1`, is
the number of child of `C` and `1` its number of metadata. The next number `0` is the number of
child of `D` and it has `1` metadata, which is `99`. `C`, as seen above, has one metadata, so `2` is
`C`’s metadata. Then, since `A` has three metadata, `1`, `1` and `2` are its.

Pfiou. Seems hard to read for a human, right? However, if you’re used a bit to recursive data
structure and more specifically recursive parsers, this kind of encoding is actually pretty neat!

Let’s go and implement the parser of that tree. First, the structure. We will not need the header in
the output (it’s only used for parsing), so we will not encode that directly (it’s still available
as the length of the list of children and length of the list of metadata entries):

```
data Node = Node {
    nodeChildren :: [Node],
    nodeMetadata :: NonEmpty Natural
  } deriving (Eq, Show)
```

Pretty simple, right? This is a self-recursing data structure that is pretty simple and basic to any
functional programmer.

> The `NonEmpty a` data type, in Haskell, is a list that cannot have zero element. That is enforced
> at compilation as it’s impossible to create such a list without explicitly giving at least one
> element. All the operations defined on `NonEmpty a` respect that rule (for instance, removing an
> element from it might not remove anything if it has only one element – otherwise you’d break its
> invariant and Haskell is not Javascript. [Lol](https://phaazon.net/media/uploads/inner_troll.png).

So, how do we parse that? I’ve already spoiled you the solution: we need to implement a recursive
parser. I know that because I’ve been using [parsec] for like 7 years now, so I’m pretty used to
that kind of parsing and as you use it, you will quickly recognize when you can use such an idiom.

However, instead of using [parsec] directly, I will implement it myself with some very basic types
every Haskellers know – if you don’t: go learn them! I’ll be using the `State` type only, which is
basically *just* a recursive function used in a monadic fancy way:

```
-- A possible representation of the State monad is just a function that takes a value 's' and
-- returns a new, altered 's' (we call that a state, but as you can see, it’s completely pure code,
-- no mutation happens at all) and an output value 'a'.
data State s a = State { runState :: s -> (s, a) }
```

> The real `State` type is defined in the [mtl](http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html#t:State)
> Haskell library.

So here, you have the *impression* or *illusion* of `s` being a state, but what it truly is is just
a value that is passed around to a recursive function – and plot twist: recursion is the way to
implement locally-defined states, but I will not explain that (read my [netwire tutorial] and the
`Auto` type, if you are interested).

The functor / monadic part:

```
instance Functor (State s) where
  fmap f = State . fmap (fmap f) . runState

instance Applicative (State s) where
  pure x = State (, x)
  p <*> q = State $ \s ->
    let (s', f) = runState p s
        (s'', a) = runState q s'
    in (s'', f a)

instance Monad (State s) where
  return = pure
  q >>= f = State $ \s -> let (s', a) = runState q s in runState (f a) s'
```

> All of this can be generated automatically by GHC with `deriving` data annotation.

And some combinators we’ll need:

```
-- Get the current value of the “state”.
get :: State s s
get = State $ \s -> (s, s)

-- Get the current value of the “state” with a function pre-applied to it.
gets :: (s -> a) -> State s a
gets = flip fmap get

-- Change the value of the “state” by applying a function to the state.
modify :: (s -> s) -> State s ()
modify f = State $ \s -> (f s, ())

-- Just a convenient method to just get the output value and discard the final state. You need the
-- initial value to use as state.
evalState :: State s a -> s -> a
evalState st = snd . runState st
```

So basically, since this is a very basic and simple code (I think all Haskellers should write that
in their first month using Haskell, it’s a good exercise), I just included the `mtl` library and
used its `State` type to write my recursive parser.

This is my parser:

```
newtype Parser a = Parser { runParser :: State [Natural] a } deriving (Applicative, Functor, Monad)
```

So basically, a `Parser a` generates value of type `a` and maintains a list of `Natural` around.
Those `Natural` are the numbers from the input we are going to parse. Let’s write the actual parser
now.

```
-- Turns the (string-encoded) list of numbers and generates the root node, that contains all of
-- the children.
parse :: String -> Node
parse = evalState (runParser parseNode) . map read . words

-- Read a single number from the input and consume it from the state.
readInput :: Parser Natural
readInput = Parser $ gets head <* modify tail

parseNode :: Parser Node
parseNode = do
  -- We read the two first numbers (header)
  childrenNb <- readInput
  metadataNb <- readInput

  -- Recursive parser! The NE.fromList is an unsafe function that is used for convenience for this
  -- puzzle part.
  children <- replicateM (fromIntegral childrenNb) parseNode
  metadata <- fmap NE.fromList (replicateM (fromIntegral metadataNb) readInput)

  pure $ Node children metadata
```

As you can see, the parser code is extremely simple with a recursive combinator parser! And we’re
actually done for the first part. The checksum is simple and is:

```
checksum :: Node -> Natural
checksum node = metadataChecksum (nodeMetadata node) + sum (map checksum $ nodeChildren node)

metadataChecksum :: NonEmpty Natural -> Natural
metadataChecksum = sum . NE.toList
```

## Part 2

The second part is not interesting as it just requires a new method to compute the “value” of a
given node:

```
nodeValue :: Node -> Natural
nodeValue (Node [] metadata) = metadataChecksum metadata
nodeValue (Node children metadata) = sum [nodeValue n | Just n <- map index (NE.toList metadata)]
  where
    index i =
      let i' = fromIntegral i - 1
      in if i' < 0 || i' >= length children then Nothing else Just (children !! i')
```

[Haskell solution](https://github.com/phaazon/advent-of-code-2k18/blob/master/day-08/src/Main.hs)

# Day 9: Marble Mania

[Text](https://adventofcode.com/2018/day/9

## Part 1 & 2

This puzzle was the first one when I decided to go full Rust! All the remaining puzzles were solved
in Rust – if you were reading only for Haskell, sorry for your loss. :(

This puzzle is not really interesting as it’s just a fancy algorithm that adds element to a
collection and sometimes removes from it. There was a trick, though: the second part requires to run
our algorithm on an input that was a hundred times larger.

The typical trap is that when you add value in the middle of a collection, the complexity in terms
of memory and CPU can largely vary. Everything depends on what you do. For very rare additions /
deletions, it’s possible that you can accept *O(n)* complexities. However, if you often insert
stuff, you might want something else. In the same spirit, some data structure can efficiently add
in *O(1)* at the beginning or end of the collection or might require a complete copy.

Even though the puzzle is not interesting in itself, it reminds us how crucial and critical it is
that a programmer must know what structure to use depending on the inputs **and operations that will
be performed on the data**. In our case, we are going to add and remove *a lot* at arbitrary places
in the collection. Vectors are really bad candidates at that kind of operations, because they will
require a complete scan of the right part of the collection, which is *O(n)*, every time you add
or delete something (to shift right / left, respectively). This is bad. Vectors are also bad when
you want to add at its beginning (it requires the same right shift as the random case).

Double-ended queue ([`VecDeque`](https://doc.rust-lang.org/std/collections/struct.VecDeque.html) in
Rust) are a solution to the problem to insert at the beginning. That insertion is *O(1)* amortized.

My solution to this was to use a [zipper] to stay focus on a given number in the collection but also
“move around” in *O(1)*. The idea is the following:

```
struct Zipper {
  left: VecDeque<isize>,
  middle: isize,
  right: VecDeque<isize>,
}
```

When you want to move to the left, you just take the `middle` number and `push_front` it to the
`right` double-ended queue and you `pop_back` the `left` one and that value becomes the new
`middle`. You do the opposite thing to go to the right.

To insert an element at the current location, you just `push_front` `middle` to the `right` and then
`middle` is assigned the new value. To remove, you just `pop_front` `right` into `middle`.

Then, the all puzzle is just adding and removing according to a predicate. Since all the operations
I mentioned above run in *O(1)* amortized (they might allocate if the buffer is too small), we will
not suffer from the typical *O(n²)* complexity a `Vec` implementation has.

[Rust solution](https://github.com/phaazon/advent-of-code-2k18/blob/master/day-09/src/main.rs)

# Day 10: The Stars Align

[Text](https://adventofcode.com/2018/day/10)

## Part 1

This is the kind of problem I suck the most at. Not because they’re hard. Because they’re easier
than expected. As an engineer, I tend to overthink about the context, the input’s hidden properties,
the possible errors, the heuristics, what could go wrong, etc. On a regular job basis, that is
actually a good thing – it’s better to foresee things than to have to repair them. However, at a
given extreme, that way of thinking will make you go nuts and will make you think of an easy and
straightforward problem as a complex and convoluted one. I know that and the main thing my mind does
when solving a problem is to think about how **really hard** a problem is. I just completely failed
on this one, haha. :D

So, you are given a short list (~360) of 2D points. You know nothing about how they’re spread nor
the limits they lie in. You **only** have ~360 points on a 2D plane. Those points, however, come
with two attributes:

  - Their positions, as a pair of relative numbers ([`-∞; +∞]`).
  - Their (constant) velocities, as a pair of relative numbers as well ([`-∞; +∞]`).

So basically, each point starts at a given position and goes into a straight line forever. The unit
of the velocity is not known and is not really needed – even though it might be `unit/s`.
Nevertheless, the text gives the hinting that, at a given (unknown) time, the points form a message
that can be visually witness. The goal is to give the message.

In the first 15 minutes, I went through several thoughts. “Whoa, they want us to write an [OCR]?!
Really?!”. Nah, you dumbass. Since we all have a unique input (and hence, a unique expected output),
we don’t have to write an algorithm that can recognize any text. We just have to get to visualize
our input.

However, when does the text appear? At `t = 0`, we have a large cloud of spread points. We don’t
know when the text will form. Also, the challenge explicitly states that the text forms only once:
the points will never gather into text afterwards. We must not miss it then.

My idea was that to find hidden properties of the overall text first. By being able to extract a
useful information telling me whether or not I’m far or close from having a visualizable text, I was
able to run a loop-like simulation, moving each points by its respectiv velocities, until that
hidden information reaches a local minimum. As an engineer, I was annoyed by that, because I had no
idea whether the first local minimum was the right one – the puzzle’s text doesn’t state anything
about that and I had not found any information to help with that in the input. I could also use the
wrong criteria (maybe we’re looking for a local maximum?). I got stuck with those ideas for long
minutes.

Finally, I decided to implement a specific criteria:

  - At each simulation step, we compute the [AABB] that encloses all the input points that have
    moved.
  - If that [AABB]’s [area] is less then the previous one, we store the [area], the width and height
    of the [AABB] and the current time of the simulation. We also print those values on the standard
    output.
  - If not, we continue.

When I ran that loop, I got the first local minimum in **10011** seconds. Clearly, if you tried to
actually run that simulation with the real time, you’d be waiting for a long time – **10011**
seconds is 2 hours, 46 minutes and 51 seconds.

The size of the [AABB] at `t = 10011` was also pretty small (around **60×60**). I then decided to
display the message directly in the console. In order to do that, I had to transform my 2D points
(expressed in the natural ℝ² basis we use in *world space* coordinates) into a space that I could
easily use to display (basically, `[0; w]` and `[0; h]`). That transformation is done with the
following code:

```
// The rendered “map”
let mut lol = vec!['.'; w as usize * h as usize];

for p in points {
  let x = (p.position.0 - aabb.lower.0) * (w - 1) / w;
  let y = (p.position.1 - aabb.lower.1) * (h - 1) / h;
  let o = x + y * w;

  lol[o as usize] = '#';
}
```

Then, we just need to iterate on all the points and render them to the terminal to finish the
challenge:

```
for row in 0 .. h {
  for col in 0 .. w {
    print!("{}", lol[(col + row * w) as usize]);
  }

  println!("");
}
```

## Part 2

Part 2 was almost a joke: we were asked to give the time at which the text appeared. As this was a
*hidden property* to find in the first place, completing part 2 took a few seconds: `10011`.

[Rust solution](https://github.com/phaazon/advent-of-code-2k18/tree/master/day-10/src/main.rs)

# Day 11: Chronal Charge

[Text](https://adventofcode.com/2018/day/11)

## Part 1

A pretty common algorithm to implement: sliding window. Basically, you are given a matrix of numbers
and you have to compute several sums using a sliding *kernel* which size is *3×3*. The size of the
matrix is *300×300* and you just want to compute the biggest *3×3* square (and give its index in the
matrix as row / column).

This was my solution:

```
let mut largest = (0, i8::min_value()); // (index, power)

// 298 is 300 - 2: we want to stop there so that the 3×3 square won’t overflow
for row in 0 .. 298 {
  for col in 0 .. 298 {
    let mut power = 0;

    // sum the square
    for i in 0 .. 3 {
      for k in 0 .. 3 {
        power += grid[index(col + i, row + k)];
      }
    }

    let i = index(col, row);

    // if its power is any larger, store it along with its index
    if (power == largest.1 && i < largest.0) || power > largest.1 {
      largest = (i, power);
    }
  }
}

println!("Largest fuel cell: ({}, {})", 1 + largest.0 % 300, 1 + largest.0 / 300);
```

That’s pretty much it. Second part is more interesting.

## Part 2

For this part, the problem changes a bit: we still want to sum squares, but we want to get the find
the square that has the largest total power of any size comprised between *1×1* and *300×300* – we
want its index and its size.

That problem can be solved in several ways, with different complexities. It’s easy to see that you
can quickly go with a bad complexity if you decide to refactor the previous algorithm to take a
dimension (that will be squared) and call it 300 times. Maybe that would be enough.

However, I wanted to implement something smarter on this one. It’s easy to see that a lot of
spanning squares will overlap. For instance:

```
+-+-+-+-+-+
|0|1|2|3|4|
+-+-+-+-+-+
|6|7|8|9|A|
+-+-+-+-+-+
|B|C|D|E|F|
+-+-+-+-+-+
```

If you consider the first, top-leftmost *2×2* square:

```
+-+-+
|0|1|
+-+-+
|6|7|
+-+-+
```

And the top-left-mostmost *3×3* square:

```
+-+-+-+
|0|1|2|
+-+-+-+
|6|7|8|
+-+-+-+
|B|C|D|
+-+-+-+
```

You can see that a the smaller one is included in the bigger one. What it means is that each
spanning square is a partial sum to spanning square of a higher dimension. My algorithm benefits
from that in order to reduce the number of elements to sum at each given dimension.

Also, another thing I did that suprised people on IRC: I reversed the way the algorithm works in
terms of traversal. Instead of traversing dimensions and then traversing the grid (for all
dimensions then for all squares), I traverse the grid and then I traverse the dimensions (for all
square in the grid, for all the dimensions). This gives me a more natural way to write the partial
sums in my code.

Finally, I also work out some formalæ to know “what’s the biggest dimension we can go up to given
the current grid cell.” Yeah, think twice: when you want to go through all dimensions from the
top-leftmost cell, you will be able to sum squares from *1×1* up to *300×300*. But which dimension
can you go to when the starting (*1×1*) cell is in the middle of the grid? This is actually pretty
easy. The formalæ can be found very quickly by thinking in terms of size of the grid (*300×300*) and
the index of a grid cell. The biggest dimension is just the minimum of the maximal allowed row
iterations and the maximal allowed column iterations. You can picture that mentally by “which edge
I am the closest to?”. For rows, it’s simply `300 - current_row` and for columns,
`300 - current_column`. The minimum value gives you the maximal spanning dimension you can go up to.

Finally, a word on how the partial sums are created: when you start computing the sum of the `N`
dimension, you already have the partial sum of dimension `N-1` (the algorithm starts with the first
dimension set to a given value). Then, instead of summing `N²` element, since you already have the
sum of `(N-1)²`, you just need to sum *2 × (N - 1) + 1* values. If you’re not convinced, at
dimension `278`, `278² = 77284` sums while my algorithm is `2 × (278 - 1) + 1 = 555` sums. It’s
around **139 times less**.

In my code, I do that by adding – relative to the previous spanning square – the right column
(which size is *N - 1*), the bottom line (*N - 1* as well) and the single element in the diagonal.
Hence `2 × (N - 1) + 1`. And that completes a new partial sum, that will be used for higher
dimensions!

Here’s just a very quick schema to show you how to compute the sum at dimension `5` by using the
sum of the spanning square of dimension `4` – `·` is already computed and `R` are the right column,
`B` the bottom line and `D` the element in the diagonal:

```
+-+-+-+-+-+
|·|·|·|·|R|
+-+-+-+-+-+
|·|·|·|·|R|
+-+-+-+-+-+
|·|·|·|·|R|
+-+-+-+-+-+
|·|·|·|·|R|
+-+-+-+-+-+
|B|B|B|B|D|
+-+-+-+-+-+
```

So, here’s the code:

```
let mut largest2 = (0, i64::min_value(), 0); // (index, power, dimension)

// for all rows…
for row in 0 .. 300 {
  let max_iter_row = 300 - row; // 300 -> 1

  // for all columns…
  for col in 0 .. 300 {
    let max_iter_col = 300 - col; // 300 -> 1
    let max_dim_squared = max_iter_row.min(max_iter_col); // 300x300 -> 1x1

    // power used for nested dimensions
    let mut nested_power = grid[index(col, row)] as i64;

    // note: we don’t have to compute the first dimension because it’s set right away to the given
    // nested power

    // for all dimensions up to the max
    for d in 1 .. max_dim_squared {
      let mut power = nested_power;

      // compute the 2 × (N - 1) elements
      for k in 0 .. d {
        power += grid[index(col + d, row + k)] as i64;
        power += grid[index(col + k, row + d)] as i64;
      }

      // add the diagonal
      power += grid[index(col + d, row + d)] as i64;

      let i = index(col, row);

      if (power == largest2.1 && i < largest2.0) || power > largest2.1 {
        largest2 = (index(col, row), power, d + 1);
      }

      nested_power = power;
    }
  }
}

println!("Largest fuel cell of all: ({}, {}, {}, of power {})", 1 + largest2.0 % 300, 1 + largest2.0 / 300, largest2.2, largest2.1);
```

[Rust solution](https://github.com/phaazon/advent-of-code-2k18/tree/master/day-11/src/main.rs)

# Day 12: Subterranean Sustainability

[Text](https://adventofcode.com/2018/day/12)

## Part 1

This puzzle looked a bit like the double-ended queue one from day 9. The extra bit of information
is that you now have to apply a pattern on several values to know how they should be mutated. Given
a list of flower pots and some rules that give you how a pot should grow flowers (or not) according
to the state of itself and its neighbors, the goal is to predict the sum of the pots (their index in
the list) for all pots that contain flowers after **20 generations**.

In the first place, I had to recognize that I needed a double-ended queue. As always, the puzzle’s
text doesn’t explicitly tell you that the pots at leftmost and rightmost positions can “spawn” new
pots by applying the rules on empty pots (infinite). I was confused at that for a while.

My encoding of rules is pretty simple and wasteful: since a rule gives you a pattern (which pots)
and an output (should have flowers / shouldn’t), a single byte should be enough for that (the length
of a rule is five: it gives you the state of the two left neighbors, the state of the current pot
and the state of the two right neighbors). However, I encoded those with a simple array of five
binary states (`Rule::Empty` and `Rule::Pot`).

In order to apply a pattern, we must retreive the current pot and two at its left position and two
at its right position (if pots are missing because we’re at edges, we must spawn empty pots with
negative / positive indices). Then we can just apply the pattern by looking it up in a hashmap: we
get the next generation value.

Nothing really interesting code-wise to show here.

## Part 2

Part 2 is funny. We’re told that instead of finding the sums after **20 generations**, we need to
find it **after fifty billion (50000000000) generations.** Obviously, trying to run the above
algorithm for 50000000000 generations will take ages, so we need to find a better way.

My first inital idea was that if I took a look at the actual sum value at each generation, I could
– perhaps – see some kind of patterns. At first I was looking for cycles and hence cycling sums. I
then run my algorithm and had a look at the output data. I was suprised to find that, very quickly,
the flowers grow linearily. What it means is that, after a given number of generations, you can
guess how many flowers there will be at a given future generation by applying a linear formula
(typically, a simple multiplication and addition).

In my case, I noticed that at generation `100`, the sum was `6346`. At `101`, it was `6397`. At
`102`, it was `6448`. At `200`, it was `16546`. You can see the pattern – if you don’t, compute the
difference between the sum at `101` and the sum at `100`… and the difference of sum at `102` and
`101`.

Hence, I came up with the following linear formula:

```
// O(1) get the score at a given generation – works only for gen ≥ 100.
fn score_at(gen: u64) -> u64 {
  6346 + (gen - 100) * 51
}
```

The actual implementation uses `101` instead of `100` because we want to get the sum **after** a
given number of generations, not **at**.

That kind of linear optimization was really fun to write yet a bit tricky to find. :)

[Rust solution](https://github.com/phaazon/advent-of-code-2k18/blob/master/day-12/src/main.rs)

# Day 13: Mine Cart Madness

[Text](https://adventofcode.com/2018/day/13)

## Part 1

I think this was the puzzle I enjoyed the most – among the ones I did. The goal is to parse is rails
map on which wagons go and make wagons move around by respecting some specific rules: we must find
wagon collision is report their positions 2D position.

Parsing is actually pretty simple: the input data is the 2D map that contains the rail system along
with the initial position of wagons. About the map, I decided to store only crossings, not the
actual rails, because I didn’t need them! So in order to do so, I changed a bit the regular way I
encode 2D maps in memory and decided to use a hashmap!

```
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Rail {
  Cross, // +
  RampRight, // /
  RampLeft // \
}

struct Map(HashMap<(u32, u32), Rail>);
```

The rest of the code is actually pretty simple: there are several functions, one for moving carts,
one for changing directions at cross, one for detecting collision. A main `loop` is responsible in
moving carts and checking if there’s any collision. If no collision is detected, we just loop back
up. If a detection is detected, we break the loop and display the position of the crash. The
collision algorith returns the IDs of the carts that collided into each other.

```
let collision = loop {
  // sort the cart by y component
  carts.sort_by(|a, b| a.pos.cmp(&b.pos));

  let collisions = move_carts(&map, &mut carts);
  if !collisions.is_empty() {
    break collisions[0];
  }
};

println!("First collision: {:?}", carts[collision.0].pos);
```

The `sort_by` is needed because of priority rules in the puzzle’s text.

Moving carts and detecting collision is pretty straightforward:

```
fn move_carts(
  map: &Map,
  carts: &mut Vec<Cart>,
) -> Vec<(usize, usize)> {
  let mut collisions: Vec<(usize, usize)> = Vec::new(); // no collision to begin with

  // move and check collision for all carts
  'outer: for i in 0 .. carts.len() {
    // check that this cart hasn’t been collided into yet
    for &collision in &collisions {
      if i == collision.0 || i == collision.1 {
        // already collided, don’t move that
        continue 'outer;
      }
    }

    // move the cart and check if it’s collided into another
    move_cart(map, &mut carts[i]);
    let collision = find_collision(&carts, i);

    if let Some(collider) = collision {
      collisions.push((collider, i));
    }
  }

  collisions
}
```

This code is not really optimized – we redo the same thing very often – but it’s way than enough to
solve that puzzle’s part. Finding collision is very simple: we just try to find a cart with the
same position.

The tricky part is for moving at cross. The rules state that if you arrive at a cross, you have to
turn in a given direction and change the future direction you will take at the future cross, if any.
This was encoded inside each cart, so that they have a *“memory”* of turns to take.

```
#[derive(Debug)]
struct Cart {
  pos: (u32, u32),
  dir: Direction,
  next_turn: Turn
}
```

A cart starts by going on its (relative!) `Turn::Left`, then at the next turn it will go
`Turn::Straight`, then `Turn::Right` and finaly will loop back to `Turn::Left`. Note how different
it is to `Direction`: a `Turn` is relative to the current movement of a cart while a `Direction` is
absolute (at first, I wanted to have `North`, `East` etc. for `Direction` so that [confusion is not
possible](https://kaamelott-soundboard.2ec0b4.fr/#son/de_tout_facon_on_dit_le_nord_selon_comment_on_est_tourne_ca_change_tout)).

## Part 2

In that part, we want to find the last standing cart, assuming that crashing carts are immediately
removed from the map. The code is actually very similar: instead of breaking the loop at the first
collision, we break it when there’s only one cart left on the map – we don’t forget te remove the
crashed carts!

```
loop {
  // sort the cart by y component
  carts.sort_by(|a, b| a.pos.cmp(&b.pos));

  for (ci, ck) in move_carts(&map, &mut carts) {
    carts = carts.into_iter().enumerate().filter(|&(i, _)| i != ci && i != ck).map(|(_, c)| c).collect();
  }

  if carts.len() == 1 {
    break;
  }
};

println!("Last standing cart: {:?} ", carts); // this contains only one cart
```

[Rust solution](https://github.com/phaazon/advent-of-code-2k18/blob/master/day-13/src/main.rs)

# Day 14: Chocolate Charts

[Text](https://adventofcode.com/2018/day/14)

## Part 1

Very similar to the double-ended queue puzzle as well, this one doesn’t actually require any
deletion, just indexing correctly into a growing buffer. Nothing really interesting to show about
this problem, except maybe the way recipes are created.

> To create new recipes, the two Elves combine their current recipes. This creates new recipes from
> the digits of the sum of the current recipes' scores. With the current recipes' scores of 3 and 7,
> their sum is 10, and so two new recipes would be created: the first with score 1 and the second
> with score 0. If the current recipes' scores were 2 and 3, the sum, 5, would only create one
> recipe (with a score of 5) with its single digit.

In order to implement that, I recognized that I will always create at least one recipe: `0 + 0 = 0`,
and as soon as I create two recipes, the other one will always be `1`, because the maximum value is
`9 + 9 = 18 (1 and 8)`. Here’s my code that gets those two recipe numbers:

```
fn create_new_recipe(a: usize, b: usize) -> (Option<usize>, usize) {
  let s = a + b;
  let x = s / 10;
  let y = s % 10;

  (if x == 1 { Some(x) } else { None }, y)
}
```

## Part 2

Part 2 is really not interesting as it’s just using `ends_with` to find a suffix. I’ll let you read
the code if you’re interested.

[Rust solution](https://github.com/phaazon/advent-of-code-2k18/blob/master/day-14/src/main.rs)

# Day 15: Beverage Bandits

[Text](https://adventofcode.com/2018/day/15]

# Part 1

Aaaaaaaaaand this is the last puzzle I attempted. I actually decided not to finish it because it was
taking me time. I will tell you more about that in the conclusion.

The goal is to write a simulation of elves fighting goblins (or goblins fighting elves) and finding
paths in a map that has holes / mountains in it. So most of the code to write is about [Dijkstra] or
[A*]. The puzzle seemed interesting but it was way too much for my spare time to spend on. I advise
you to have a look at the insanely long puzzle’s text – that will give you an idea of everything you
need to implement in order to get the your solution working.

# Conclusion

Ah, my first Advent of Code. It was both interesting, exciting, frustrating and time-consuming. I
found several pros and drawbacks:

Pros, first:

  - It’s all fun. Since I live in France, I couldn’t compete with people who get the puzzles
    unlocked in the morning around 10:00 AM while they were unlocked around 5:00 AM in France – I
    have a job, I have to, you know… SLEEP! So I just did it for fun.
  - Writing algorithms to solve math and operational problems is always interesting and makes it a
    great training.
  - Comparing solutions on IRC was also pretty fun!

And drawbacks:

  - The fact west-europeans cannot compete (the timezone issue).
  - The difficulty of the puzzles is not correctly balanced. Some puzzle are insanely simple to
    solve and some others take you hours (if not days if you don’t have a lot of spare time).
    Sometimes I felt frustrated at this, because I’ve been lacking sleep time for weeks and had to
    go to bed in order not to destroy my physical health. I’m curious to hear about other
    west-europeans: how did you manage your time, social life and work life with AoC?
  - Some problems had a very long first part 2 and the second part was really stupid (read
    [The Stars Align – Part 2](#part-2-8)). I would have preferred to have more consistency or more
    parts, for instance.

My general feeling is that it was fun, but I think that I won’t do it next year, because I had to
put all my spare projects on hold for that. I didn’t learn anything new – all the algorithms had me
write algorithms I already knew, except maybe the partial dimension squared algorithm I “invented”:
someone told me that it’s very similar to a real and well-known algorithm! How funny is that! The
algorithm is [Summed-area table](https://en.wikipedia.org/wiki/Summed-area_table) and my solution
is, indeed, very similar to it. But the thing is: I came up with the idea, and this is priceless for
training brains!

Now I’ll return to my graphics, Rust and over experiment projects of mine! I hope you liked that
article, it was a bit long (it took me almost two weeks to write!) but I felt I needed to make it.
To… well… scrap and forget about Advent of Code and all my spare time I didn’t use for my own
projects. :)

Keep the vibes – especially you, [\@lqd].

[Advent of Code]: https://adventofcode.com/about
[netwire tutorial]: https://phaazon.net/blog/getting_into_netwire
[zipper]: https://wiki.haskell.org/Zipper
[referential transparency]: https://wiki.haskell.org/Referential_transparency
[AABB]: https://en.wikipedia.org/wiki/Bounding_volume
[Manhattan distance]: https://en.wikipedia.org/wiki/Taxicab_geometry
[parsec]: http://hackage.haskell.org/package/parsec
[OCR]: https://en.wikipedia.org/wiki/Optical_character_recognition
[area]: https://en.wikipedia.org/wiki/Area
[Dijkstra]: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
[A*]: https://en.wikipedia.org/wiki/A*_search_algorithm
[\@lqd]: https://github.com/lqd
