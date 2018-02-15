# Bezier curves

It’s been a while I’ve been wanting to do that. Now it’s done!
[smoothie](https://hackage.haskell.org/package/smoothie), a Haskell package for
dealing with curves and splines, updated to version
[0.3](https://hackage.haskell.org/package/smoothie-0.3).

That version introduces several changes. If you’re a good programmer, you might
already have noticed that the major version got incremented. That means there’re
compatibility breaking changes. If you don’t know what I’m talking about, you
should definitely read
[this](https://wiki.haskell.org/Package_versioning_policy).

The first – non-breaking – change is that the package now supports
[Bézier](https://en.wikipedia.org/wiki/B%C3%A9zier_curve) interpolation! I’ve
been reading about **Bézier curves** for a while because there’re very present
and important for animation purposes – think of **Blender**. Feel free to
dig in the documentation on hackage for further details.

The second – breaking – change is that the interface has changed, especially the
implementation of *splines*. However, the interface is now simpler and doesn’t
require a lot of change in your code if you’ve been using older versions.

Feel free to read the
[CHANGELOG](https://hackage.haskell.org/package/smoothie-0.3/changelog) for
technical hints.

As always, tell me what you think of the library, and keep the vibe!
