# splines-1.0.0: the first Release Candidate

Oh hello there! It’s been a while since I wrote on here. My last article was posted on February
25th and then I decided it was time to blog more about what I do. Especially, I think I will try to
blog more often, even if it’s about trivial things, as long as I share my thoughts.

So today, I’m going to talk about the [splines] crate. And more specifically, the [splines-1.0.0-rc.1]
release candidate I uploaded today on crates.io.

## Foreword: you said splines?

Maybe you’re wondering what a spline is, in the first place. A spline is a mathematic curve that is
defined by several polynomials. You can picture them mentally by several small and simple curves
combined to each others, giving the curve an interesting shape and properties. Now why we want
splines is easy to understand: imagine a curve, something smooth and a bit complex (maybe even with
loops). Now, imagine you want to make an object move along that curve. How do you represent that
curve and how to you “make something advance along it?”

Splines are defined by several “control points”, also known “keys”, “knots” or whatever makes sense
to you. Those are just points of interest that will have a direct impact on how the curve will bend
and behave. Most of control points have the interesting property that the curve **will** pass
through them, allowing you to define points you want your object to pass by, for instance. Not all
control points have that property, though.

Now all the magic happens with those control points: when you want to sample a value at a given
control point, you just get back the value carried by this control point… but when you want to
sample in between two control points, for instance, you get back an *interpolated value*. That
interpolation is the key to why we want splines. Several interpolation mechanisms exist and I’m sure
you know at least two:

  - **Constant interpolation**: it means that given two control points, sampling at the first
    control point or any part between the control points will give you the carried value of the
    first control point. You will get the carried value of the second control point if you sample at
    its sampling value. That might be like a useless feature but it is very handy when designing and
    implementing “stop-by” movement with camera.
  - **Linear interpolation**: this one you might know it, especially if you attended math courses at
    high-school. Linear interpolation is often referred to as “drawing a straight line”. And
    typically, “everything linear” can often graphed with a straight line! Thank about linear
    complexity (*O(n)*) and what it looks like on a graph. Linear interpolation that gives you a
    strict blending of arguments depending at which distance you sample from them. If sample at
    `t = 0`, you get 100% of the first value and 0% of the second value. If you sample at `t = 0,5`,
    you get an equal amount of both value (so you get the mean between both value). At `t = 1`, you
    get 0% of the first value and 100% of the second value. Simple but really useful.
  - **Cosine interpolation**: this one is known by people who are used to animation, graphics,
    motion design, etc. Cosine interpolation gives you a smoother interpolation than the linear one
    and provides a first derivative that is more interesting (i.e. it translates to *acceleration*).
  - Others: [splines] provide you with some other interpolation modes and some (that I really want!)
    are missing. For instance, cubic [Bézier interpolation] is something I want and that I already
    have coded in the Haskell version of the [splines] crate. It should be easy to add but so far,
    no one has asked for them and I don’t need them, so… ;)

> Keep in mind that a spline is a just an abstractio mathematical object and that you can use it for
> lots of concepts and situations. For instance, in demoscene productions of mine, I use splines for
> camera movement, animation transitions, color-blending and more!

## The splines crate

The [splines] crate has been around for some time now and I received some feedback from people on
the [GitHub page](https://github.com/phaazon/splines) asking for more features. Basically, the crate
can now:

  - Support failible sampling more sanely (instead of panicking on some very trick situations, you
    get a `None`).
  - Support for polymorphic sampling types (you have `f32` but also `f64` or whatever).

I think something interesting to dissect in this blog article is the last feature about polymorphic
sampling types and what I had to do in order to make it happen. Also, this is highly correlated to
a design choice I have made months ago: supporting foreign crates to interpolate specific types is
done inside the [splines] crate, not in other crates. I will discuss why just bellow.

## Polymorphic sampling

Basically, in [splines], prior to `1.0.0`, a spline was polymorphic type mapping a key of type `f32`
to a control point of type `Key<V>`, `V` being the carried value. You then manipulate a `Spline<V>`.

[splines]: https://crates.io/crates/splines
[splines-1.0.0-rc.1]: https://crates.io/crates/splines/1.0.0-rc.1
[Bézier interpolation]: https://en.wikipedia.org/wiki/B%C3%A9zier_curve
