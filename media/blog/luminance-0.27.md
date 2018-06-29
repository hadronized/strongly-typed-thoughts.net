[luminance-0.27] was released lately and a bunch of new and interesting things was added.

# Easier API

The [CHANGELOG] is available to read but one of the most important change was the refactoring of
the pipeline system and of the overall API to make it less confusing. For instance, the
`Framebuffer::default` is now renamed `Framebuffer::back_buffer`. Another example is the *face
culling*, that was in pre-0.27 enabled by default (causing people who don’t now about it to be
confused because of a black screen). This is now disabled by default; hence less confusion.

The pipeline system types are now easier to use. For instance, redundancy was removed with the
various `Bound*` objects. When you want a *bound buffer of `f32`* for instance, you just have to
use the type `BoundBuffer<'a, f32>` instead of the pre-0.27 `BoundBuffer<'a, Buffer<f32>>`. That is
a small change but it will eventually make working with luminance more comfortable.

Another change that I think is beneficial is the new `TessSlice` type – that replaces the pre-0.27
`TessRender`. This type represents a *GPU tessellation slice*. Such a slice can be used to render
part of a tessellation. What’s great is that the legacy methods to build such slices
(`TessRender::one_whole` for instance) have a new, more generalized way to do it:
`TessSliceIndex<Idx>`. This trait gives you a `slice` method which argument has type `Idx`. Several
implementors exist:

  - `impl TessSliceIndex<Range<usize>> for Tess`.
  - `impl TessSliceIndex<RangeFrom<usize>> for Tess`.
  - `impl TessSliceIndex<RangeTo<usize>> for Tess`.
  - `impl TessSliceIndex<RangeFull<usize>> for Tess`.

They all give you a `TessSlice` to work with (with the appropriate lifetime). The four
implementations listed above can be invoked with:

  - `tess.slice(0..10)`.
  - `tess.slice(0..)`.
  - `tess.slice(..10)`.
  - `tess.slice(..)`.

For those who wonder:

> *Why have you not implemented [Index] instead?*

The current [Index] trait doesn’t give you enough power to index a value by returning something else
than a direct reference. Everything is summed up [in the RFC I wrote to fix Index].

Feel free to have a look at the [CHANGELOG] for further information.

# Examples, examples, examples!

One of the most important feature people have asked is adding examples. The online documentation is
not enough – even if I spent lots of hours enhancing it. So I added several examples:

  - **01-hello-world**: learn how to draw two colored triangles by using vertex
    colors (comes in *direct* and *indexed* geometry versions).
  - **02-render-state**: learn how to change the render state to change the way the
    triangles are rendered or how fragment blending happens.
  - **03-sliced-tess**: learn how to slice a single GPU geometry to dynamically
    select contiguous regions of it to render!
  - **04-shader-uniforms**: send colors and position information to the GPU to
    add interaction with a simple yet colorful triangle!
  - **05-attributeless**: render a triangle without sending any vertex data to the
    GPU!
  - **06-texture**: learn how to use a loaded image as a luminance texture on the GPU!
  - **07-offscreen**: get introduced to *offscreen rendering*, a powerful technique
    used to render frames into memory without directly displaying them on your screen. Offscreen
    framebuffers can be seen as a generalization of your screen.

You can find them in [this place](https://github.com/phaazon/luminance-rs/blob/master/CHANGELOG.md).

Please do contribute if you thing something is missing, either by opening an issue or by opening a
PR. Any kind of contribution is highly welcomed!


[luminance-0.27]: https://crates.io/crates/luminance/0.27.1
[CHANGELOG]: https://github.com/phaazon/luminance-rs/blob/master/CHANGELOG.md#0270
[Index]: https://doc.rust-lang.org/std/ops/trait.Index.html
[in the RFC I wrote to fix Index]: https://github.com/rust-lang/rfcs/pull/2473
