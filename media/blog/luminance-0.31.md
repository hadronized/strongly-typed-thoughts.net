[luminance] has been having lots of activity lately. I’ve been working (hard) on making it to the
`1.0` release for weeks now and came to a very important realization: lots of changes have been
done and I’m still not ready to release it.

# Motivation

Lately, [luminance] has received a wide range of feature additions, bug fixes and design ideas. I’ve
been wanting to release all those changes as part of the `1.0` release but the thing is: among all
the changes, some have been around on the `master` branch for months without a proper release on
crates.io… because the `1.0` milestone was not reached. People have been moving towards [luminance]
more and more and some provided feedback about using [luminance]. Happy but indecisive about what to
do, I faced a dilemma:

  - Either wait and release everything as part of `1.0` but eventually _block_ people from using all
    the cool features of [luminance] because the last release on crates.io is months old.
  - Break the feature set into two releases: `1.0` for everything and `0.31` for all the new
    candies.

I have decided to go with the second option.

# luminance-0.31

Just before writing this article, the [last luminance version] was `0.30` — just for the record,
that version is eleven months old, ~200 commits behind `master`. The new and most recent version is
thus `0.31` and crates got updated:

  - [luminance-0.31]
  - [luminance-windowing-0.3]
  - [luminance-glfw-0.6]
  - [luminance-derive-0.1], **a new crate** that brings procedural macros for the `#[derive(..)]`
    construct.
  - [luminance-glutin-0.1], **a new crate** supporting the [glutin] windowing crate. Currently, not
    all features are implemented but it’s usable.

Lot of work has been accomplished and I received several contributions from people all around the
globe, including PRs and issues. I’d like to remind that I appreciate both and you are **really**
encouraged to contribute in _any way you want to_. Special thanks fly to:

  - [\@ambihelical](https://github.com/ambihelical)
  - [\@linkmauve](https://github.com/linkmauve)
  - [\@bobtwinkles](https://github.com/bobtwinkles)
  - [\@twetzel59](https://github.com/twetzel59)
  - And all others whom I discussed with and who provided feedback, especially on Reddit.

About that last point, my Reddit and Twitter interactions about [luminance] have been _very_
interesting because I got to test the water about how people feel about [luminance], especially
when compared to (not so) similar crates, such as [gfx], [glium] or even [gl]. I came to the
realization, after reading people and what they would love to have as a graphics crate, that
[luminance] _can_ have the role of the _easy_ crate, that is not necessarily the fastest but fast
enough to be quickly productive. I cannot help it but keep thinking about a simple question: if
some people can make games in Java or C# with a _garbage collector_ or using old tech like
_OpenGL 2.1_ (yes, some people still make pretty good games with that), why would one need a
perfect zero-cost abstraction down-to-the-metal unsafe ultra-optimized crate to write something?
See, I went round and squares about that topic, because I’ve already used [luminance] in several
projects of mine (mostly demoscene purposes), and _it just does the job_. So, yes, [luminance]
doesn’t have that cool, new and modern _Vulkan-type_ API, I must confess. But it has its own API,
which is very functional-based (see the [History](https://github.com/phaazon/luminance-rs#history)
section of [luminance] for further details about that) and, to me, modern enough so that people
don’t get frustrated with the overall design being too clunky.

So, yeah. I gave up on the [idea of introducing backends](https://phaazon.net/blog/pre-luminance-n-random-thoughts)
in [luminance]. I really changed my mind several times about that topic and it’s actually when I
read comments on Reddit about people getting confused about the direction of the crate that I made
up my mind: [luminance] must remain simple. Having a system of backends that can be hot-switched,
concurrent etc. is just going to make things hard for people to use it — and maintain it! It’s
likely that I will introduce, however, _feature-gates_ to allow to compile [luminance] on
WebAssembly via WebGL, _OpenGL ES_ or even Vulkan at some point, but the difference is that no backend
will be implemented. That means that those feature-flags aren’t likely to be summable at first. But
all of this will be done in future releases; stay tuned.

The current blog post brings a description of all the changes of [luminance-0.31]. It should be the
last `0.*.*` version before hitting the `1.0` release. To the question:

> _Why not releasing `1.0` directly?_

I answer that the `1.0` milestone has a backlog with two major changes that will take time to
implement and I think it would be a pity to postpone a lot of great changes that are already
available because of two features that are yet to be implemented. The concept of versions is to
allow releasing features in a structured way without having to wait too much. So here we are.

This post also show cases a small tutorial about how to get started with [luminance]. The very first
steps you should have would be to have a look at the [examples/] directory and try to play with all
of the samples.

> Disclaimer: the following section of this article is based on
> [luminance’s changelog](https://github.com/phaazon/luminance-rs/blob/master/luminance/CHANGELOG.md#031).

## Bug fixes

  - Fix and remove `panic!` and attributeless renders.
  - Various internal bug fixes and performance improvements.
  - Fix pixel code for `Format::R` and `Format::RG` when querying a texture’s texels.

## Major changes

  - Remove the concept of `GTup`. No code was using it and it was not really elegant.
  - Remove the `uniform_interface!` macro and replace it with the `UniformInterface` procedural
    derive macro.
  - Buffer mapping is now always a `mut` operation. That is required to _lock-in_ the mapped slices
    and prevent to generate new ones, which would be an undefined behavior in most graphics backends
    such as _OpenGL_.
  - Change the framebuffer’s slots types and meanings. Those are now more natural to use (for
    instance, you don’t have to repeat the framebuffer’s associated types and dimensions nor even
    use the `Texture<..>` type anymore, as a type family is now used to ease the generation of
    color and depth slots).
  - Change the way the `Vertex` trait is implemented.
    - The `Vertex::vertex_format` method has been renamed `Vertex::vertex_desc`.
    - Instead of returning a `VertexFormat`, that method now returns a `VertexDesc`. Where a
      `VertexFormat` was a set of `VertexComponentFormat`, a `VertexDesc` is a set of
      `VertexBufferDesc`.
    - `VertexBufferDesc` is a new type that didn’t exist back then in _0.30_. It provides new data
      and information about how a vertex attribute will be spread in a GPU buffer. Especially, it has:
      - An _index_, allowing to map the vertex attribute in a shader.
      - A _name_, used by shader programs to perform mapping.
      - An _instancing_ parameter, used to determine whether we want **vertex instancing**.
      - A `VertexAttribDesc`, the new name of `VertexComponentFormat`.
    - As said above, `VertexComponentFormat` was renamed `VertexAttribDesc`.
    - Vertex attribute can now be _normalized_ if they are _signed integral_ or _unsigned integral_.
      That is encoded in the `VertexAttribType`’s integral variants.
    - A new trait has appeared: `VertexAttrib`. Such a trait is used to map a type to a
      `VertexAttribDesc`.
    - `Vertex` has zero implementor instead of several ones in _0.30_. The reason for that is that
      `VertexBufferDesc` is application-driven and depends on the _vertex semantics_ in place in the
      application or library.
    - Vertex semantics are introduced in this release and are represented via the `Semantics` trait.
      Implementing directly `Semantics` is possible, even though not recommended. Basically,
      `Semantics` provides information such as the _index_ and _name_ of a given semantics as long
      as the list of all possible semantics, encoded by `SemanticsDesc`.
    - Users are highly advised to look at the `Vertex` and `Semantics` proc-macro derive in the
      [luminance-derive] crate.
  - Revise the `Tess` type to make it easier to work with.
    - The `Tess::new` and `Tess::attributeless` functions were removed.
    - The `TessBuilder` type was added and replace both the above function.
    - That last type has a lot of methods that can be combined in different ways to build powerful
      situation of tessellations, among (but not limited to):
      - Normal and indexed tessellations.
      - Attributeless tessellations.
      - Tessellations with vertex instancing support.
      - Deinterleaved tessellations
      - Tessellations with support for _primitive restart indexing_.
    - Slicing was revised too and now has support for two new Rust operators:
      - The `a ..= b` operator, allowing to slice a `Tess` with inclusive closed bounds.
      - The `..= b` operator, allowing to slice a `Tess` with inclusive bounds open on the left
        side.
    - Previously, the `Tess::new` associated function expected indices to be a slice of `u32`. This
      new release allows to use any type that implements the `TessIndex` trait (mapping a type to a
      `TessIndexType`. Currently, you have `u8`, `u16` and `u32` available.
    - Add `Tess::{as_index_slice,as_index_slice_mut}`. Those now enable you to conditionally slice-map
    the _index buffer_ of a `Tess`, if it exists.
  - Add support for generic texture sampling.
    - This new feature is supported thanks to the `SamplerType` trait, used as constraint on the
      `Pixel::SamplerType` associated type.
    - Basically, that feature allows you to bind a `Floating` texture without caring about the
      actual type. That is especially true as you typically use `sampler2D` in a shader and not
      `sampler2DRGB32F`.
    - Such a feature reduces the number of combination needed to refactorize code.
  - Implement _vertex attrib explicit binding_. This is a huge change that is related to _vertex
    semantics_. Basically, in _0.30_, you have to ensure that the `layout (location = _)` is
    correctly set to the right value regarding what you have in your `Tess`’ vertex buffers. That
    was both _unsafe_ and terribly misleading (and not very elegant). The new situation, which
    relies on _vertex semantics_, completely gets rid of _vertex locations_ worries, which get
    overrided by [luminance] when a shader program gets linked.
  - Change boolean-like `enum`s — such as `DepthTest` — variants from `Enabled` / `Disabled` to
    `On` / `Off` or `Yes` / `No`, depending on the situation.
  - Move `swap_buffers` from `GraphicsContext` to `Surface` in [luminance-windowing].
  - Switch to `GenMipmaps` instead of `bool` to encode whether mipmaps should be generated in
    texture code. That change is a readability enhancement when facing texture creation code.
  - Make `Dimensionable::zero_offset()` a constant, `Dimensionable::ZERO_OFFSET`.
  - Change the way cursor modes are encoded from `bool` to `CursorMode`.

## Minor changes

  - Add the [luminance-glutin] crate, the windowing crate support for [glutin].
  - Add the [luminance-derive] crate.
    - That crate provides several procedural derive macros you can use to easily implement all
      required traits to work with [luminance]. Especially, some traits that are `unsafe` can be
      implemented in a safe way with that crate, so you should definitely try to use it.
    - Current available proc-macros are:
      - `#[derive(Vertex)]`: derive the `Vertex` trait for a `struct`.
      - `#[derive(Semantics)]`: derive the `Semantics` trait for an `enum`.
      - `#[derive(UniformInterface)]`: derive the `UniformInterface` trait for a `struct`.
  - Support for dynamic uniform queries. Those are used whenever you don’t know which variables will
    be used in a shader at compile-time. This might be the case if you’re writing a GUI tool or a
    video game that uses a custom scripting language / node-ish representation of shaders. That
    feature doesn’t break the already-in-place and great uniform interface but complements it. You
    can use a shader `Program<_, _, ()>` and still set uniform values by querying the uniforms
    dynamically. This feature also fully benefits from the strongly typed interface of `Uniform<_>`,
    so you will get `TypeMismatch` runtime error if you try to trick the type system.
  - Add the `std` feature gate, allowing to compile with the standard library – this is enabled by
    default. The purpose of this feature is to allow people to use `default-features = false` to
    compile without the standard library. This feature is currently very experimental and shouldn’t
    be used in any production releases so far – expect breakage / undefined behaviors as this
    feature hasn’t been quite intensively tested yet.
  - Add support for the `R11FG11FB10F` pixel format.
  - Migrate to Rust Edition 2018.
  - The `WindowOpt` now has support for multisampling. See the `WindowOpt::set_num_samples` for
    further details.
  - Implement dynamic edition of windowing types properties. That allows to change data on-the-fly,
    such as the cursor mode.
  - Introduce normalized texturing. That feature is encoded as pixel formats: any pixel format which
    symbol’s name starts with `Norm` is a _normalized pixel format_. Such formats state that the
    texels are encoded as integers but when fetched from a shader, they are turned into
    floating-point number by normalizing them. For instance, when fetching pixels from a texture
    encoded with `R8UI`, you get integers ranging in `[0; 255]` but when fetching pixels from a
    texture encoded with `NormR8UI`, even though texels are still stored as 8-bit unsigned integers,
    when fetched, you get floating-point numbers comprised in `[0; 1]`.

## Patch & misc changes

  - Remove `std::mem::uninitialized` references, as it is now on deprecation path. Fortunately, the
    codes that were using that function got patched with safe Rust (!) and/or simpler constructs.
    It’s a win-win.
  - Add the `#[repr(C)]` annotation on vertex types in examples. That is a bit unfortunate because
    such an annotation is very likely to be mandatory when sending data to the GPU and it should be
    done automatically instead of requiring the user to do it. That situation will be fixed in a
    next release.
  - Add more CI testing.
  - Update examples and made them available to the `cargo run --example` command. Read more
    [here](./examples/README.md).
  - Massive documentation rewrite (among the use of `#![deny(missing_docs)]`. The situation is still
    not perfect and patch versions will be released to fix and update the documentation. Step by
    step.
  - Add design notes and documents in the repository.
  - Massive dependencies update. Special thanks to @eijebong for his help!
  - Add the `11-query-texture-texels` example, which showcases how to query a texture’s texels and
    drop it on the filesystem.
  - Add and update _README_ files. Especially, the gitter link was removed and an IRC link was
    added. If you want to get help:
    - Server: `irc.freenode.net`
    - Channel: `#luminance`

And of course, keep the vibes!

[last luminance version]: https://crates.io/crates/luminance/0.30.1
[luminance]: https://crates.io/crates/luminance
[luminance-0.31]: https://crates.io/crates/luminance/0.31.0
[luminance-windowing-0.3]: https://crates.io/crates/luminance-windowing/0.3.0
[luminance-glfw-0.6]: https://crates.io/crates/luminance-glfw/0.6.0
[luminance-derive-0.1]: https://crates.io/crates/luminance-derive/0.1.0
[luminance-glutin-0.1]: https://crates.io/crates/luminance-glutin/0.1.0
[glutin]: https://crates.io/crates/glutin
[gfx-hal]: https://crates.io/crates/gfx-hal
[glium]: https://crates.io/crates/glium
[gl]: https://crates.io/crates/gl
[examples/]: https://github.com/phaazon/luminance-rs/blob/master/luminance/examples/README.md
