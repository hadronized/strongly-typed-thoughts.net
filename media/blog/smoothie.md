# The smoother the better!

It’s been a while I haven’t written anything on my blog. A bit of refreshment
doesn’t hurt much, what do you think?

As a [demoscener](http://en.wikipedia.org/wiki/Key_frame), I attend
demoparties, and there will be a very important and fun one in about a month.
I’m rushing on my 3D application so that I can finish something to show up, but
I’m not sure I’ll have enough spare time. That being said, I need to be able to
represent smooth moves and transitions without any tearing. I had a look into a
few **Haskell** *spline* libraries, but I haven’t found anything
interesting – or not discontinued.

Because I do need splines, I decided to write my very own package. Meet
[smoothie](https://hackage.haskell.org/package/smoothie), my **BSD3** Haskell
spline library.

## Why splines?

A *spline* is a curve defined by several polynomials. It has several uses, like
vectorial graphics, signal interpolation, animation tweening or simply plotting
a spline to see how neat and smooth it looks!

Splines are defined using polynomials. Each polynomials is part of the curve and
connected one-by-one. Depending on which polynomial(s) you chose, you end up
with a different shape.

For instance, 1-degree polynomials are used to implement straight lines.

![](http://phaazon.net/pub/linear_spline.png)

As you can see, we can define a few points, and interpolate in between. This is
great, because we can turn a *discrete* set of points into lines.

Even better, we could use 3-degree polynomials or cosine functions to make each
part of the spline smoother:

![](http://phaazon.net/pub/spline.png)

We still have discrete points, but in the end, we end up with a smooth set of
points. Typically, imagine sampling from the spline with time for a camera
movement. It helps us to build smooth moves. This is pretty important when doing
animation. If you’re curious about that, I highly recommend having a look into
[key frames](http://en.wikipedia.org/wiki/Key_frame).

## Using splines in Haskell

So I’ve been around implementing splines in Haskell the most general way as
possible. However, I don’t cover – yet? – all kinds of splines. In order to
explain my design choices, I need to explain a few very simple concepts first.

### Sampling

A spline is often defined by a set of points and polynomials. The first point
has the starting *sampling* value. For our purpose, we’ll set that to *0*:

```haskell
let startSampler = 0
```

The sampling value is often `Float`, but it depends on your spline and the use
you want to make of it. It could be `Int`. The general rule is that it **should
be orderable**. If we take two sampling values `s` and `t`, we should be able
to compare `s` and `t` (that’s done through the typeclass constraint `Ord` in
Haskell).

So, if you have a spline and a sampling value, the idea is that sampling the
spline with `startSampler` gives you the first point, and sampling with
`t` with `t > startSampler` gives you another point, interpolated using points
of the spline. It could use two points, three, four or even more. It actually
depends on the polynomials you use, and the interpolating method.

In [smoothie](https://hackage.haskell.org/package/smoothie), sampling values
have types designed by `s`.

### Control points

A spline is made of points. Those points are called **control points** and
[smoothie](https://hackage.haskell.org/package/smoothie) uses `CP s a` to refer
to them, where `s` is the sampling type and `a` the carried value.

Although they’re often used to express the fact that the curve should pass
through them, they don’t have to lie on the curve itself. A very common and
ultra useful kind of spline is the B-spline.

![](http://phaazon.net/pub/b_spline.png)

With that kind of spline, the property that the curve passes through the control
points doesn’t hold. It passes through the first and last ones, but the ones
in between are used to *shape* it, a bit like **magnets** attract things around
them.

Keep in mind that **control points** are very important and used to define the
main aspect of the curve.

### Polynomials

Polynomials are keys to spline interpolation. They’re used to *deduce* sampled
points. Interpolation is a very general term and used in plenty of domains. If
you’re not used to that, you should inquiry about
[linear interpolation](http://en.wikipedia.org/wiki/Linear_interpolation) and
[cubic interpolation](http://en.wikipedia.org/wiki/Cubic_Hermite_spline), which
are a very good start.

Polynomials are denoted by `Polynomial s a` in
[smoothie](https://hackage.haskell.org/package/smoothie), where `s` and `a` have
the same meaning than in `CP s a`.

## Getting started with smoothie

### Types and constraints

[smoothie](https://hackage.haskell.org/package/smoothie) has then three important
types:

  - `CP s a`, the control points
  - `Polynomial`, the polynomials used to interpolate between control points
  - `Spline s a`, of course

The whole package is parameterized by `s` and `a`. As said earlier, `s` is very
likely to require an `Ord` constraint. And `a`… Well, since we want to represent
points, let’s wonder: which points? What kind of points? Why even *“points”*?
That’s a good question. And this is why you may find
[smoothie](https://hackage.haskell.org/package/smoothie) great: it doesn’t
actually know anything about points. It accepts **any kind of values**. Any?
Almost. Any values that are in an **additive group**.

> *“What the…”*

I won’t go into details, I’ll just vulgarize them so that you get quickly your
feet wet. That constraint, when applied to **Haskell**, makes `a` to be
an endofunctor – i.e. `Functor` – and additive – i.e. `Additive`. It also
requires it to be a first-class value – i.e. its kind should be `* -> *`.

With `Functor` and `Additive`, we can do two important things:

  - First, with `Functor`. It enables us to lift computation on the inner type.
    We can for instance apply a single function inside `a`, like `*k` or `/10`.
  - Then, with `Additive`. It enables us to add our types, like `a + b`.

We can then make **linear combinations**, like a*k + b*q. This property is well
known for [vector spaces](http://en.wikipedia.org/wiki/Vector_space).

The fun consequence is that providing correct instances to `Functor` and
`Additive` will make your type useable with
[smoothie](https://hackage.haskell.org/package/smoothie) as carried value in the
spline! You might also have to implement `Num` and `Ord` as well, though.

### Creating a spline

Creating a spline is done with the `spline` function, which signature is:

```haskell
spline :: (Ord a, Ord s) => [(CP s a, Polynomial s a)] -> Spline s a
```

It takes a list of **control points** associated with **polynomials** and
outputs a **spline**. That requires some explainations… When you’ll be sampling
the spline,
[smoothie](https://hackage.haskell.org/package/smoothie) will look for which
kind of interpolation method it has to use. This is done by the lower nearest
control point to the sampled value. Basically, a pair `(cp,polynomial)` defines
a new point and the interpolation method to use for the curve ahead of the
point.

Of course, the latest point’s polynomial won’t be used. You can set whatever you
want then – protip: you can even set `undefined` because of laziness.

Although the list will be sorted by `spline`, I highly recommend to pass a
sorted list, because dealing with unordered points might have no sense.

A control point is created by providing a sample value and the carried value.
For instance, using [linear](https://hackage.haskell.org/package/linear)’s `V2`
type:

```haskell
let cp0 = CP 0 $ V2 1 pi
```

That’s a control point that represents `V2 1 pi` when sampling is at `0`. Let’s
create another:

```haskell
let cp1 = CP 3.341 $ V2 0.8 10.5
```

Now, let’t attach a polynomial to them!

#### Hold that for me please

The simplest polynomial – wich is actually not a polynomial, but heh, don’t look
at me that way – is the 0-degree polynomial. Yeah, a constant function. It takes
the lower control point, and *holds* it everwhere on the curve. You could
picture that as a staircase function:

![](http://phaazon.net/pub/hold_spline.gif)

You might say that’s useless; it’s actually not; it’s even pretty nice. Imagine
you want to attach your camera position onto such a curve. It will make the
camera *jump* in space, which could be desirable for flash looks!

Use the `hold` `Polynomial` to use such a behavior.

#### Oh yeah, it’s straightforward!

1-degree functions often describe *lines*. That is, `linear` is the `Polynomial`
to use to connect control points with… straight lines.

#### Have fun on the unit circle!

One very interesting `Polynomial` is `cosine`, that defines a cosine
interpolation, used to smooth the spline and make it nicer for moves and
transitions.

#### Highway to the danger zone

If you’re crazy, you can experiment around with `linearBy`, which, basically, is
a 1-degree polynomial if you pass `id`, but will end up in most complex shapes
if you pass another function – `(s -> s)`. Dig in documentation on *hackage* for
further information.

#### Sampling our spline

Ok, let’s use a linear interpolation to sample our spline:

```haskell
let spl = spline [(cp0,linear),(cp1,hold)]
```

> *Note: I used `hold` as a final polynomial because I don’t like using
`undefined`.*

Ok, let’s see how to sample that.
[smoothie](https://hackage.haskell.org/package/smoothie) exports a convenient
function for sampling:

```haskell
smooth :: Ord s => Spline s a -> s -> Maybe a
```

`smooth spl s` takes the sampling value `s` and maybe interpolate it in the
`spl` spline.

> *“Maybe? Why aren’t you sure?”*

Well, that’s pretty simple. In some cases, the curve is not defined at the
sampling value you pass. Before the first point and after, basically. In those
cases, you get `Nothing`.

## That’s not the end

I wrote [smoothie](https://github.com/phaazon/smoothie) in a few hours, in a
single day. You might have ideas. I want it to be spread and widely used by
awesome people. Should you do graphics programming, sound programming, animation
or whatever implying splines or smoothness, please provide feedback!

For people that would like to get contributing, here’s the
[github page](https://github.com/phaazon/smoothie) and the
[issue tracker](https://github.com/phaazon/smoothie/issues).

If no one comes up with, I’ll try to add some cubic interpolation methods, like
*hermitian splines*, and one of my favorite, the famous **Catmull Rom** spline
interpolation method.

As always, have fun hacking around, and keep doing cool stuff and sharing it!
