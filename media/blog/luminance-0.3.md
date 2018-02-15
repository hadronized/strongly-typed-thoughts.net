# Unleashing the power of textures!

From [luminance-0.1](https://hackage.haskell.org/package/luminance-0.1) to
[luminance-0.2](https://hackage.haskell.org/package/luminance-0.2) included, it was not possible to
use any texture types different than two-dimensional textures. This blog entry tags the new
release, [luminance-0.3](https://hackage.haskell.org/package/luminance-0.3), which adds support for
several kinds of texture.

## A bit more dimensions

[`Texture1D`](https://hackage.haskell.org/package/luminance-0.3/docs/Graphics-Luminance-Texture.html#t:Texture1D),
[`Texture2D`](https://hackage.haskell.org/package/luminance-0.3/docs/Graphics-Luminance-Texture.html#t:Texture2D) and
[`Texture3D`](https://hackage.haskell.org/package/luminance-0.3/docs/Graphics-Luminance-Texture.html#t:Texture3D)
are all part of the new release. The interface has changed – hence the breaking changes yield a
major version increment – and I’ll explain how it has.

Basically, textures are now fully polymorphic and are constrained by a typeclass:
[`Texture`](https://hackage.haskell.org/package/luminance-0.3/docs/Graphics-Luminance-Texture.html#t:Texture).
That typeclass enables ad hoc polymorphism. It is then possible to add more texture types without
having to change the interface, which is cool. Like everything else in luminance, you just have to
ask the typesystem which kind of texture you want, and everything will be taken care of for you.

Basically, you have three functions to know:

  - [`createTexture`](https://hackage.haskell.org/package/luminance-0.3/docs/Graphics-Luminance-Texture.html#v:createTexture),
    which is used to create a new texture ;
  - [`uploadSub`](https://hackage.haskell.org/package/luminance-0.3/docs/Graphics-Luminance-Texture.html#v:uploadSub),
    used to upload texels to a subpart of the texture ;
  - [`fillSub`](https://hackage.haskell.org/package/luminance-0.3/docs/Graphics-Luminance-Texture.html#v:fillSub),
    used to fill – *clear* – a subpart of the texture with a given value.

All those functions work on `(Texture t) => t`, so it will work with all kinds of texture.

## Cubemaps

[`Cubemap`s](https://hackage.haskell.org/package/luminance-0.3/docs/Graphics-Luminance-Texture.html#t:Cubemap)
are also included. They work like other textures but add the concept of *faces*. Feel free to dig in
the documentation for further details.

# What’s next?

I need to find a way to wrap *texture arrays*, which are very nice and useful for
*layered rendering*. After that, I’ll try to expose the change to the framebuffers so that we can
create framebuffers with cubemaps or that kind of cool feature.

In the waiting, have a good a week!
