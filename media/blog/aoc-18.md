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

![Haskell solution](https://github.com/phaazon/advent-of-code-2k18/blob/master/day-07/src/Main.hs)

# --- Day 8: Memory Maneuver ---

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

# --- Day 9: Marble Mania ---

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

# --- Day 10: The Stars Align ---

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
(expressed in the natural ℝ² basis we use in *world space* coordinates) into 

[Rust solution](https://github.com/phaazon/advent-of-code-2k18/tree/master/day-10/src/main.rs)

[Advent of Code]: https://adventofcode.com/about
[netwire tutorial]: https://phaazon.net/blog/getting_into_netwire
[zipper]: https://wiki.haskell.org/Zipper
[referential transparency]: https://wiki.haskell.org/Referential_transparency
[AABB]: https://en.wikipedia.org/wiki/Bounding_volume
[Manhattan distance]: https://en.wikipedia.org/wiki/Taxicab_geometry
[parsec]: http://hackage.haskell.org/package/parsec
[OCR]: https://en.wikipedia.org/wiki/Optical_character_recognition
[area]: https://en.wikipedia.org/wiki/Area
