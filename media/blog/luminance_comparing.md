It’s been a while since I’ve been wanting to do this. Either on IRC, Reddit, GitHub Issues, IRL or
other any other way of communicating, there are more and more people asking me to *compare
[luminance] to other, well known and famous, rendering-like frameworks*.

I’ve made a listing with pros and cons. in the [README] file of [luminance]. I’ll just copy that
section and report it below so that you’re not even a click away from the listing!

# How does it compare to…

A lot of folks have asked me on IRC / Reddit / gitter / etc. how [luminance] compares to other
famous frameworks. Here are a few comparisons:

## luminance vs. glium

[glium] has been around for a while – way longer than [luminance].

> According to crates.io, [glium] is [no longer actively developed by its original author](https://users.rust-lang.org/t/glium-post-mortem/7063).

- It has more macros to generate `impl`, which is neat – however, this tends to be less and less
  true as [luminance] has been getting new macros lately.
- It addresses the OpenGL concepts more directly. For instance, you find a `glium::VertexBuffer`
  type – a buffer to hold vertices – while [luminance] gives you a `Tess` type that hides that kind
  of complexity away for you.
- [glium] doesn’t prevent you to pick a specific version of OpenGL / GLSL if you want to. For
  instance, the [tuto-02-triangle.md] sample uses GLSL140. [luminance] is different on that
  side as it **must** be used with an **OpenGL 3.3 Core Context** – hence a GLSL330. This can be
  seen as a [luminance] drawback as it will only work with a specific version of OpenGL (GL 3.3 and
  GLSL 330).
- Drawing is done by requesting a `Frame` object in [glium] and passing the whole pipeline at once.
  The pipeline then must be completely resolved prior to render. See
  [this](https://docs.rs/glium/0.20.0/glium/trait.Surface.html#tymethod.draw). [luminance] is more
  flexible on that side since it enables you to provide the rendering pipeline in a dynamic way,
  allowing for tricky structure traversal without cloning nor borrowing, for instance. [glium]
  requires a tuple of `(vertices, indices, program, uniforms, render_state)` while [luminance] works
  by building the AST node by node, level by level.
- The program customization (uniforms) feature of [glium] is per-value while [luminance] is
  per-type. In [glium], you can change the uniforms you pass to a program by using the `uniform!`
  macro when invoking the `draw` command on your `Frame`. This has the effect to lookup the uniform
  in the shader for every call – modulo caching – and requires you to explicitly pass the uniforms
  to use. [luminance] uses a *contravariant* approach here: you give the type of the *uniform
  interface* you want to use and you will be, later, handed back a reference to an object of that
  type so that you can alter it. All of this is done at the type level; you have nothing to do when
  passing values to the shader in the pipeline. Opposite approaches, then. Also, notice that
  [luminance] looks up the uniform names exactly once – at `Program` creation – and provides several
  hints about uniform utilization (inactive, type mismatch, etc.). [glium] seems to have runtime
  type checking as well, though.
- Linear algebra seems to be handled the same way (e.g. 4×4 matrices are encoded as `[[f32; 4] 4]`
  in both crates).
- Textures are handled a bit similarily, even though, because [glium] uses a fixed pipeline, the
  user doesn’t have to do anything special but passing references around. [luminance] exposes a
  scoped API to use opaque texture values, which adds more noise. A good point for [glium] there
  since the texture passing is just about references while [luminance] has a specific API for that.
  This is the same situation for GPU buffers.
- About vertices, [glium] is more permissive about how data can be stored on the GPU. For instance,
  it supports *deinterleaved vertices* – e.g. when you have your positions in
  a GPU buffer and the normals or colors in another. [luminance] requires you to interleave your
  vertices. This is due to the fact that if you use all your vertex attributes in a shader – which
  was an assumed situation when designing the vertex API – this will provide better performances
  than using deinterleaved data (because of cache friendliness). However, for cases when you don’t
  want to use other attributes – depth peeling, shadow mapping, z-pre-pass, etc. – that might be a
  huge drawback as you’ll peek / load data from the buffer you won’t even use, killing the point of
  having a cache-friendly peek.
- Draw state (i.e. depth test, depth buffer comparison equations, culling, etc.) is done pretty much
  the same way in both crates: by creating an object representing the state when calling the
  rendering pipeline.
- [glium] supports *compute shaders* while [luminance] doesn’t (so far! ;) ).
- Additionnally, because of the dynamic nature of its pipelines, [luminance] allows for better
  sharing of resources, especially shaders and render states. This is due to the fact that
  [luminance] represents rendering pipelines as dynamic ASTs in which you can change all the
  branches and leaves on the fly. See the point about the `Frame` of [glium] above.

> Sources and tutorials for [glium] [here](https://github.com/glium/glium/tree/master/book).

## luminance vs. gfx

[gfx] is a high-performance, bindless graphics API for the Rust programming language. It aims to be
the default API for Rust graphics: for one-off applications, or higher level libraries or engines.

> The current listing concerns [gfx] in its’ *pre- low level* situation. Recent [gfx] APIs are
> different.

- The [gfx]’s scope is not the same as [luminance]’s: [gfx] abstracts over **several graphics API**.
  Among them, **OpenGL**, **Metal**, **DirectX** and will clearly attract people who want to *write
	once, release everywhere*.
- [gfx] requires you to write shader sources for all possible backends. See for instance this
  [example](https://github.com/gfx-rs/gfx/tree/pre-ll/examples/cube/shader) which uses
  GLSL, HLSL, Metal and even SPIR-V – it even uses several version of GLSL, running in GLSL 100, 120
  and 150.
- Vertex types are defined differently from [luminance]. In [gfx], you use the `gfx_defines!` macro
  to easily introduce the types and make named bindings (to map to vertex attributes in shaders). A
	lot of boilerplate is generated thanks to that macro, which is neat. [luminance] has a similar
	scheme even though the macro support in [luminance] is less advanced. On a general note:
	[luminance] is less macro-driven.
- Similarily, the uniforms are handled in the same `gfx_defines!` macro. They’re declared in very
  similar ways as in [luminance]. However, [gfx] uses the same syntax as with vertex definition,
	while [luminance] has an ad hoc approach – it’s typical Rust code with *annotations*. That doesn’t
	make a huge difference per-se. [gfx] calls *uniforms* `constant`, which is nice because it can
	abstract over the concept, while [luminance] doesn’t and uses the terms `Uniform`, `Uniformable`,
	`UniformInterface`, etc.
- The pipeline system in [gfx] is also defined in `gfx_defines!` macro invokations. This is
  seriously cool and [luminance] suffers a bit from not having that – defining pipeline in
	[luminance] is a bit boring because of lambda / closures boilerplate.
- In [gfx], the GPU buffers are handled via *factories* and smart constructors, like
  `create_vertex_buffer_with_slice`, for instance. This is nice as it enables coping with
	multi-threading safety, which is something [luminance] doesn’t support yet (it’s planned, though).
- [gfx] comes in with an *application* abstraction layer, which [luminance] doesn’t – 
  `gfx_app::Application`, defined in the [gfx_app] crate.
- [gfx] has **a lot** of running projects and is actively maintained and changed over time. For
  instance, both [ggez] and [amethyst] use [gfx].

> On a general note, [luminance] and [gfx] pre-ll are way too different to be compared to each
> other. If you want to target several backends, do not hesitate and go to [gfx]. If you target only
> one system and want simplicity, go to [luminance].

## luminance vs. ggez

[ggez] is a Rust library to create a Good Game Easily.

> As [ggez] is primarily focused on 2D projects, it doesn’t compare directly to [luminance], as it’s
> about graphics, sound, input, etc. Also, [ggez] is using [gfx].

## luminance vs. vulkano

[vulkano] is a wrapper around the Vulkan graphics API.

> The scope is not really the same as [vulkano] only targets Vulkan and – as for today – [luminance]
> is about OpenGL.

## luminance vs. amethyst

[amethyst] is a fast, data-oriented, and data-driven game engine suitable for rapid prototyping and
iteration.

> As stated in the current document, [luminance] is not a game engine. So if you’re looking for a
> tool that already has everything needed to build a game, [amethyst] should be your toy. Also, it’s
> using [gfx].

## luminance vs. piston

[piston] is a modular game engine written in Rust.

> As for other game engines, nothing to compare here since [luminance] is **not a game engine**!

## luminance vs. spectra

[spectra] is a demoscene framework / engine mainly written for demoscene purposes, even though it
can now be used to some other extents (animation / video game).

> [spectra] uses [luminance] as primary *graphics backend*, but it provides way more features than
> just graphics. You’ll find audio, animation, editing, resource system, custom shading language,
> etc.


[luminance]: https://crates.io/crates/luminance
[vulkano]: https://crates.io/crates/vulkano
[glium]: https://crates.io/crates/glium
[tuto-02-triangle.md]: https://github.com/glium/glium/blob/master/book/tuto-02-triangle.md#program
[gfx]: https://crates.io/crates/gfx
[gfx_app]: https://crates.io/crates/gfx_app
[amethyst]: https://crates.io/crates/amethyst
[ggez]: https://crates.io/crates/ggez
[vulkano]: https://crates.io/crates/vulkano
[piston]: https://crates.io/crates/piston
[spectra]: https://crates.io/crates/spectra
[README]: https://github.com/phaazon/luminance-rs#how-does-it-compare-to
