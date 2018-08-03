This article introduces the [???] crate.

# Preliminary work

I’ve been using interpolation (linear, bilinear, trilinear, B-splines, etc.) for a while now because
of my demoscene productions. My [spectra](https://crates.io/crates/spectra) crate features some
interpolation primitives and I’ve been wanting to move them out of spectra for a while.

Basically, my need is simple:

  - I use interpolation for *a lot* of situations: animation, procedural generation, editing,
    transitions, etc.
  - I need several interpolation implicit methods / spaces:
    + Constant/step.
    + Linear / bilinear / trilinear.
    + Cosine.
    + Cubic Hermite spline.
  - I also need splines with control points (knots):
    + Catmull-Rom.
    + Bézier.

That’s a lot of things. For what it’s worth, I’ll just use as reference a library I wrote years ago
in Haskell, [smoothie], doing exactly that.

However, because I didn’t want to bloat the ecosystem, I had a look around to see whether I could
simply drop my code and add a simple dependency instead of extracting things from spectra and
creating, again, new crates. Here are the reasons why.

## `bspline`

This crate is about [B-spline] only and the interface is highly unsafe (panics if out of control
points for instance). My current spectra code is already safer than that, so no reason to lose
features here.

## `spline`

This is… a… placeholder crate – i.e. it doesn’t have anything exposed. People do this to book a
crate name for later. I highly dislike that and people doing this should really be warned not to do
this, because they prevent others – who actually have some code to put there – from using the name.
Plus, for this `spline` crate, I had already have a look years ago. In years, the so-called “author”
hasn’t even uploaded anything and the name is just reserved… for nothing.

I highly suggest people from the Rust team to forbid people from doing this. This is just useless,
childish and brings zero value to the community. Just remove that crate as no one has ever had *the
chance* to ever depend on it.

## `trajectory`

This is an interesting crate, but it’s way too much specialized to me. It could be very interesting
to use it for camera and objects paths, but I also need splines for a lot of other situations, so
this is too much restrictive (you can see functions like `acceleration`, `position`, `velocity` that
works only for `T: Float`).

## `nbez`

A – looks like – very comprehensive Bezier crate, but doesn’t provide anything for all the other
interpolation I need.

# Let’s build yet another crate!

Looks like the crate I’m looking for doesn’t exist yet – oh yes it does: it’s spectra!, but I want
a crate that does interpolation only.

I could be using some of the crates listed above, add them as a bunch of dependencies to a *glue
crate* and just expose the whole thing. However, I’m not sure they will compose quite correctly and
I might just end up re-coding the whole thing.

So the crate I’ll be building will be about interpolation. You will find step (constant)
interpolation, linear / bilinear / trilinear interpolation, cosine, cubice, Catmull-Rom and Bézier
spline interpolations. The crate will be as abstract as possible to enable developers to plug in
their own types. Because the `spline` name is already taken – duh! – I’ll use the `splines` name…

> I’ll take my chance to try to take over `spline` first, though. I really don’t like that kind of
> things in an ecosystem, it’s just unaesthetic.

[smoothie]: https://hackage.haskell.org/package/smoothie
[B-spline]: https://en.wikipedia.org/wiki/B-spline
