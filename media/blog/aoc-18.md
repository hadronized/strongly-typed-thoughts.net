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

[Haskell solution](https://github.com/phaazon/advent-of-code-2k18/blob/master/day-02/src/Main.hs)

# --- Day 3: No Matter How You Slice It ---

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

# --- Day 4: Repose Record ---

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

# --- Day 5: Alchemical Reduction ---

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

I decided to use a [zipper](https://wiki.haskell.org/Zipper)-like traversal. My idea is the
following:

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

# --- Day 6: Chronal Coordinates ---

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

# --- Day 7: The Sum of Its Parts ---

[Text](https://adventofcode.com/2018/day/7)

## Part 1

Here we go again: graph theory. Fortunately for us, it’s not a hard graph puzzle. That first part is
to simply display a string that shows the order in which a graph must be traversed. If two nodes can
be traversed at the same time, the node which letter comes first alphabetically is traversed first.

I’ll just show the traversal because the rest is not really interesting for that puzzle:

```
-- The graph encodes the relationship in reverse: it maps every node their list of dependencies.
-- So if we have something like A -> [], it means that the A node doesn’t have dependencies.
type Graph = Map Step (Set Step)
type Step = Char

-- Get the list of all available steps; i.e. they don’t have any dependency.
getAvailable :: Graph -> [Step]
getAvailable gr = [step | (step, set) <- M.toList gr, S.null set]

stepAvailable :: Graph -> [Step]
stepAvailable gr = case sort (getAvailable gr) of
    [] -> []
(s:sx) -> s : stepAvailable (removeStep s gr)
```

![Haskell solution](https://github.com/phaazon/advent-of-code-2k18/blob/master/day-07/src/Main.hs)

[Advent of Code]: https://adventofcode.com/about
[referential transparency]: https://wiki.haskell.org/Referential_transparency
[AABB]: https://en.wikipedia.org/wiki/Bounding_volume
[Manhattan distance]: https://en.wikipedia.org/wiki/Taxicab_geometry
