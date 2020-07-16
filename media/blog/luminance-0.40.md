Today is the day [luminance-0.40] got released. This is a _massive_ update. Like never before in
the luminance ecosystem.

# (Short) story of what happened

A huge amount of features, bug fixes, usability / comfort enhancement, new crates and architecture
redesign are gathered in that update. First, let’s talk about the new most important feature: the
redesign.

## The backend (re)design

Before 0.40, luminance was an [OpenGL 3.3] crate. If you’re not into graphics APIs, OpenGL is a
graphics normalized specification, built by an open group (Khronos) that has been around for
decades. Because it’s an open  specification, lots of vendors can implement and provide drivers to
have OpenGL support, but also Free and OpenSource projects. The 3.3 version was old enough to
support a wide variety of devices, even old ones, while still having interesting features allowing
to do lots of useful things.

However, as time passes, new devices and technologies emerge. [WebGL 1.0], which appeared in 2011,
is to the Web what OpenGL is to desktop and console graphics programming. It is an open API
browsers can implement to provide support for accelerated 2D/3D rendering in your browser.

[WebGL 2.0], which appeared more recently, in 2017 (partial support in some browsers), provide an
API almost identical to what you find in OpenGL 3.3 (with some tricky caveats).

I’ve been wanting to have a way to write “luminance code”, and have it compile for other
platforms than desktops, like Web and mobile (Android and iOS). With the previous architecture,
since luminance was bound to OpenGL 3.3, it was not possible. Here comes the new archicture.

## Backends… backends? backends!

The whole luminance crate was rewritten so that its public-facing interface is completely agnostic
of what _executing GPU code_ is about. No OpenGL anymore. Instead, it depends on a type variable,
referred as `B` in luminance’s types, which must satisfy a set of conditions to be considered a
_backend_.

A backend is an implementation of the expectations made on the public-facing interface. For
instance, you can `set` a GPU buffer by providing an index and a value. That’s the public facing
part. However, what happens on the GPU is left to the backend implementation.

This new way of doing split the luminance crate into several ones:

- [luminance], which is the core, more abstract and common crate to whole. You use types coming
  from this crate to explain _what you want to do_. How it’s done is made by other crates.
- [luminance-gl], which is the OpenGL backend crate. It roughly contains a type — `GL33` — which
  can be passed around [luminance] to select the OpenGL 3.3 backend.
- [luminance-webgl], which is the WebGL 2.0 crate. I decided to go with WebGL 2.0 instead of 1.0
  because of how similar it is to OpenGL 3.3 (which was the existing code base). It’s possible to
  add support for WebGL 1.0, but it will require a lot of effort to support the feature that are
  not present in the specification.

Added to this, because of how generic and abstract [luminance] now is, people might get issues when
trying to write code that will work whatever the backend. When using the `set` functions on a
buffer, it is required that the type implements a trait from [luminance] —
`luminance::backend::buffer::Buffer`. So it’s likely the user will have to constrain types and it
might get boring. For this reason, a new crate was created: [luminance-front].

[luminance-front] allows people to write code using types from [luminance] that got their `B` type
variable replaced by a backend type at compile-time. This is done by inspecting the compilation
target, and features you enable. You are then free to write your code without worrying about
whether traits are implemented, since [luminance-front] takes care of that for you. The
re-exported types are simple aliases to [luminance]’s types, so your generic code — if you write
any — will be compatible.

## Changelog, migration guide

The changelog is quite massive for a single update. I wish I had the opportunity to split it into
several updates, but the redesign meant a lot of changes, and I got several very interesting PRs
and feature requests. Among very new stuff:

- A new platform crate has appeared: [luminance-sdl2], which adds support for the [sdl2] crate.
- [luminance-webgl] and [luminance-web-sys], to support the Web!
- [luminance-front], which is a _front_ crate to ease working with [luminance] types.
- The type system experience has been greatly improved. Most of the time, you will not have to
  annotate types anymore — like `Program` or `Tess`.
- About `Tess`, a BIG update has landed, has it’s now heavily typed (vertex type, index type,
  vertex instance data type, memory interleaving type).
- More render states features, such as the possibility to enable or disable depth writes, separate
  RGB/alpha blending, etc. etc.

A huge thanks to all the people who contributed. It means a lot to me! The list of changes is big,
so I’ll leave you with the [changelog here](https://github.com/phaazon/luminance-rs/blob/master/luminance/CHANGELOG.md#040).

> Disclaimer: this is the [luminance] changelog, but all the crates have their own, too.

Because that update is massive and has some breaking changes, I have also decided to include a
_migration guide_. The migration guide is a section in [luminance]’s changelog to explain all
the things you need to do to migrate from luminance-0.39 to luminance-0.40. **Please: if you find
something that is missing or you’re struggling with, please open an issue, a PR, or ping me on
whatever platform you like**.

## What’s next to read

The `Tess` type now supports slicing deinterleaved memory in very, very elegant ways, checked
at compile-type and type-driven. The whole new `Tess` type design is so new that a blog article
is on its way about that topic, as I find it very interesting.

While testing the redesign and new features, I have implemented a [Conway’s Game of Life] for
fun — no code available yet. I plan to remake it from scratch and record the whole experience
on video. That will be a new format and I want to hear from people and know whether they like the
format.

Finally, the [book] was also updated. The three chapters have been updated and some features have
been added — like aspect ratio in chapter 3; I don’t understand why I haven’t talked about that
earlier!

The next steps for luminance and myself, besides the blog articles and video, are… getting some
rest. I plan to get back to demoscene production, and I’m 100% sure there’s already a lot to do
with that current release of [luminance]. I plan to experiment with new backends as well, such as
an OpenGL 2.0 backend (if possible), to support really old hardware; an OpenGL 4.6 backend for
modern hardware (easy), as I already know that API pretty well; an OpenGL ES backend for mobile
development, and later, a [Vulkan] and, perhaps, a [WebGPU] backends.

Stay tuned for the `Tess` article, the Game of Life video… and keep making super fun video games,
animation and toys with [luminance]!

_And keep the vibes!_

[luminance]: https://crates.io/crates/luminance
[luminance-0.40]: https://crates.io/crates/luminance/0.40.0
[luminance-gl]: https://crates.io/crates/luminance-gl
[luminance-webgl]: https://crates.io/crates/luminance-webgl
[luminance-web-sys]: https://crates.io/crates/luminance-web-sys
[luminance-front]: https://crates.io/crates/luminance-front
[luminance-sdl2]: https://crates.io/crates/luminance-sdl2
[sdl2]: https://crates.io/crates/sdl2
[OpenGL 3.3]: https://www.opengl.org
[WebGL 1.0]: https://www.khronos.org/registry/webgl/specs/latest/1.0
[WebGL 2.0]: https://www.khronos.org/registry/webgl/specs/latest/2.0
[Conway’s Game of Life]: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
[book]: https://rust-tutorials.github.io/learn-luminance
[Vulkan]: https://www.khronos.org/vulkan
[WebGPU]: https://gpuweb.github.io/gpuweb
