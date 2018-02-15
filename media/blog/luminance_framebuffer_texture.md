I’m happily surprised that so many **Haskell** people follow
[luminance](https://github.com/phaazon/luminance)! First thing first, let’s tell
you about how it grows.

Well, pretty quickly! There’s – yet – no method to make actual renders, because
I’m still working on how to implement some stuff (I’ll detail that below), but
it’s going toward the right direction!

# Framebuffers

Something that is almost done is the
[framebuffer](https://www.opengl.org/wiki/Framebuffer_Object) part. The main
idea of *framebuffers* – in **OpenGL** – is supporting *offscreen renders*, so
that we can render to several framebuffers and combine them in several fancy
ways. Framebuffers are often bound *textures*, used to pass the rendered
information around, especially to *shaders*, or to get the pixels through
texture reads CPU-side.

The thing is… **OpenGL**’s *framebuffers* are tedious. You can have incomplete
framebuffers if you don’t attach textures with the right format, or to the wrong
attachment point. That’s why the *framebuffer* layer of **luminance** is there
to solve that.

In **luminance**, a `Framebuffer rw c d` is a framebuffer with two formats. A
*color* format, `c`, and a *depth* format, `d`. If `c = ()`, then no color will
be recorded. If `d = ()`, then no depth will be recorded. That enables the use
of *color-only* or *depth-only* renders, which are often optimized by GPU. It
also includes a `rw` type variable, which has the same role as for `Buffer`.
That is, you can have *read-only*, *write-only* or *read-write* framebuffers.

And of course, all those features – having a *write-only* *depth-only*
framebuffer for instance – are set through… **types**! And that’s what is so
cool about how things are handled in **luminance**. You just tell it what you
want, and it’ll create the required state and manage it for you GPU-side.

## Textures

The format types are used to know which textures to create and how to attach
them internally. The textures are hidden from the interface so that you can’t
mess with them. I still need to find a way to provide some kind of access to the
information they hold, in order to use them in shaders for instance. I’d love to
provide some kind of *monoidal* properties between framebuffers – to mimick
[gloss](https://hackage.haskell.org/package/gloss) `Monoid` instance for its
[Picture](https://hackage.haskell.org/package/gloss-1.9.2.1/docs/Graphics-Gloss-Data-Picture.html#t:Picture)
type, basically.

You can create textures, of course, by using the `createTexture w h mipmaps`
function.  `w` is the *width*, `h` the *height* of the texture. `mipmaps` is the
number of *mipmaps* you want for the texture.

You can then upload *texels* to the texture through several functions. The
basic form is `uploadWhole tex autolvl texels`. It takes a *texture* `tex` and
the `texels` to upload to the whole texture region. It’s your responsibility to
ensure that you pass the correct number of texels. The `texels` are represented
with a polymorphic type. You’re not bound to any kind of textures. You can pass
a list of texels, a `Vector` of texels, or whatever you want, as long as it’s
`Foldable`.

It’s also possible to fill the whole texture with a single value. In **OpenGL**
slang, such an operation is often called *clearing* – clearing a *buffer*,
clearing a *texture*, clearing the *back buffer*, and so on. You can do that
with `fillWhole`.

There’re two over functions to work with subparts of textures, but it’s not
interesting for the purpose of that blog entry.

## Pixel format

The cool thing is the fact I’ve unified pixel formats. *Textures* and
*framebuffers* share the same pixel format type (`Format t c`). Currently,
they’re all phantom types, but I might unify them further and use `DataKinds` to
promote them to the type-level. A format has two type variables, `t` and `c`.

`t` is the underlying type. Currently, it can be either `Int32`, `Word32` or
`Float`. I might add support for `Double` as well later on.

`c` is the channel type. There’re basically five channel types:

- `CR r`, a red channel ;
- `CRG r g`, red and green channels ;
- `CRGB r g b`, red, green and blue channels ;
- `CRGBA r g b a`, red, green, blue and alpha channels ;
- `CDepth d`, a depth channel (special case of `CR`; for depths only).

The type variables `r`, `g`, `b`, `a` and `d` represent *channel sizes*.
There’re currently three kind of *channel sizes*:

- `C8`, for 8-bit ;
- `C16`, for 16-bit ;
- `C32`, for 32-bit.

Then, `Format Float (CR C32)` is a red channel, 32-bit float – the **OpenGL**
equivalent is `R32F`. `Format Word32 (CRGB C8 C8 C16)` is a *RGB* channel with
red and green 8-bit unsigned integer channels and the blue one is a 16-bit
unsigned integer channel.

Of course, if a pixel format doesn’t exist on the **OpenGL** part, you won’t be
able to use it. Typeclasses are there to enforce the fact pixel format can be
represented on the **OpenGL** side.

# Next steps

Currently, I’m working hard on how to represent vertex formats. That’s not a
trivial task, because we can send vertices to **OpenGL** as
interleaved – or not – arrays. I’m trying to design something elegant and safe,
and I’ll keep you informed when I finally get something. I’ll need to find an
interface for the actual render command, and I should be able to release
something we can actually use!

By the way, some people already tried it (Git HEAD), and that’s amazing! I’ve
created the `unstable` branch so that I can push unstable things, and keep the
master branch as clean as possible.

Keep the vibe, and have fun hacking around!
