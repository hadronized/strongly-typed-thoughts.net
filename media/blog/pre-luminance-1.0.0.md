# Random thoughts on making luminance-1.0.0

Hello people. It’s been weeks I have started to work on luminance-1.0.0. For a brief recap,
[luminance] is a graphics crate that I originally created in Haskell, when I ripped it off from a
*demoscene engine* called [quaazar] in order to make and maintain tiner packages. The Rust port was
my first Rust project and it became quickly the default language I would develop graphics
applications in.

> So if you’re interested, yes, [luminance in Haskell](http://hackage.haskell.org/package/luminance)
> still exists but I maintain it for strictly minimal support (i.e. support compiler bumps, for
> instance). The default and official language to use, if you’re interested by [luminance], is Rust.

Another important thing I have to explain is the current feature list of [luminance] and what it’s
going to be soon. Currently, [luminance]’s goals are (according to the
[documentation](https://docs.rs/luminance/0.30.1/luminance/)):

  - Making the **unsafe** and **stateful** OpenGL API **safe** and **stateless**.
    - **Unsafe** means that OpenGL performs operations by relying on the fact you know what you’re
      doing: pointer arithmetic, memory alignment, etc. Lots of situations in which you can
      literally *RIP in pepperonis*.
    - **Stateful** means that lots of OpenGL functions expect a context to be in a special state for
      a correct behavior. For instance, when uploading texels to the GPU’s VRAM, the texture that
      must receive the texels must be bound to a texture unit. If you forget to do
      that, you get a weird behavior that is seen at runtime and not always correctly interpreted.
      **It is sometimes hard to know why and what something has gone wrong.** It makes also a *pain
      in the @!#=$* as soon as you want to share work on several threads.
  - Providing a simple and easy interface; that is, exposing core concepts without anything
    extra – just the bare stuff.
    - Most low-level graphics library are hard for newcomers to grasp knowledge about. [gfx-hal]
      is an excellent graphics crate (so far, I think it’s the one everyone is recommending
      everyone in the Rust gamedev community) but it is hard to play with it when you don’t know
      either OpenGL 4.5 or Vulkan — and all the required concepts might be a bit overwhelming at
      first.
    - Even though [luminance] is still a pretty low-level API (you have renderbuffers,
      framebuffers, shader stages, etc.), the interface **is opinionated** and — in my opinion —
      easier to work with. Because it is my idea and my design, the experience might be slightly
      different from e.g. [gfx-hal]. It’s neither bad or good, just different. That also responds
      to the folks who don’t understand why I’m still maintaining [luminance]: because I want to
      do graphics code my way, and more choices is always a good sign to me. Yes, we could talk
      about community efforts, but OH LOOK A BIRD!
  - Easy to read with a good documentation and set of tutorials, so that newcomers don’t have to
    learn a lot of new concepts to get their feet wet.
  - Need-driven: every piece of code added in the project must come from a real use case. If you
    feel something is missing, feel free to open an issue or even contribute!
    - This is an important aspect to me. I don’t write code for the sake of writing code (even
      libraries). I have a problem and write code to solve it. So, on the surface, [luminance]
      might seem a bit less complete that, again, e.g. [gfx-hal]. And that would be likely right.
      However, if someone likes the API and wants something in, I accept both issues and PRs. There
      are several examples of people asking for features and having me implement them. There’s also
      some folks who actually wrote the code and it’s now in [luminance]!
    - Head over to <https://github.com/phaazon/luminance-rs/issues> if you want to start discussing
      the feature set.

That’s the current situaton. A very important other aspect is that [luminance] was designed, in the
first place, to be *backend agnostic*. I quickly decided not to go this way and stick to a simpler
design with OpenGL only.

> A comparison between [luminance] and other famous crates is
> [available here](https://phaazon.net/blog/luminance_comparing).

However, I have — again! — changed my mind. Today, I want to have a system of backend in
[luminance]. Maybe not something as complex and complete as the **gfx-rs** project, but something
that will allow those backends:

  - Any **OpenGL** version: missing features from older versions will be implemented (if not too
    much work is needed) by [luminance] software patches.
  - At least one **WebGL** implementation and it must be WASM compatible.
  - **Vulkan**.

I really do not target **Metal**, **DirectX12** nor any other graphics backend you might have in
mind. However, if you think it’s possible to do so, then we should discuss about it. Nevertheless,
as I said above, [luminance] is need-driven: as I don’t need **DirectX12**, I will not work on it
unless someone REALLY needs it and provides a PR (or at least enough for me to work on it… including
beers).

# What to expect

**luminance-1.0.0** will ship… as soon as possible. The current feature set is huge, though:

  - Add the `"std"` feature-gate. This will help use [luminance] with specific contexts, especially
    for demoscene or size-limited devices. Currently, the work on `no_std` has been saved for later
    as [I had hit a bug last time I checked](https://github.com/rust-lang/rust/issues/51540).
  - Change the `Tess` interface. Now, a `TessBuilder` must be used. This allows for three major
    enhancements:
    - Vertex instancing.
    - Deinterleaved memory.
    - Builder pattern.
  - Enhance the quality and variety of examples.
  - Introduce the **luminance-derive** crate. This crate provides several proc-macros used to
    implement various `unsafe` traits easily, without writing any `unsafe` code. Among those:
    - The `Vertex` trait, used to create vertex types.
    - The `Semantics` trait, used to create vertex semantics types.
    - The `UniformInterface` trait, used to create shader *uniform interfaces*.
    - All the previous `macro_rules` are removed.
  - Support a new dynamic way to get uniforms. Sometimes, you don’t know in advance the uniforms
    your shader is going to use. When such a case arise, you might be tempted to have a minimal
    *uniform interface* — if none. *Dynamic uniform lookups* allow you to query uniforms on the
    fly. That has obviously a runtime cost but it can be handy for lots of situations (GUI editors,
    scripting, etc.).
  - Update framebuffer code so that *color slots* and *depth slots* are easier to use.
  - Introduce the concept of *drivers*. Drivers allow code to be parametered by a type which
    represents a given implementation to use for the graphics code. You can have a `GL33` driver
    for **OpenGL 3.3**, a `GL40`, `GL44`, `GL45` etc., but you can also have `WebGL` and `VK10`.
  - Add vertex instancing. Vertex instancing allows to instantiate objects by providing instance
    data directly in a `Tess` — instead of using the current method with GPU `Buffer<_>`. This was
    asked by several people and I just couldn’t ignore such an interesting and useful feature!
  - Support for deinterleaved memory. Deinterleaved memory allows for more granularity on how data
    is fetched GPU-side. With the legacy [luminance] situation, data is completely interleaved,
    which means that a vertex’s attributes all follow each other in a GPU buffer. This is both nice
    and bad: if you need to access all attributes of a vertex, this is pretty good, since
    cache-locality will be in your advantage. However, if you’re only interested in positions, for
    instance, you will waste your cache lines with attributes you will never read from!
    Deinterleaved memory stores each attributes in a different GPU memory region, allowing for a way
    better memory scheme for such uses. People using the [ECS] pattern might be used to
    deinterleaved memory.
  - Add vertex primitive restart. This allows to use tessellation indexing modes such as
    `TriangleFan` or `LineStrip` and “cut” a primitive if an index is equal to a given value. This
    is very useful for implementing terrains, quadrics or a lot of other nice things.
  - Introduce *vertex semantics*. This is one of the sexiest and most exciting feature of this next
    release. I got the idea by thinking about the fact [luminance] should be low-level but provide
    very quick access to building high-level blocks. I got the idea with vertex semantics via my
    [spectra] crate. The idea is simple: currently, we must define a `Vertex` type and the order
    in which the fields appear in the `struct` defines the bindings the GPU will present to
    shaders… but it’s the user responsibility to tell how the shader will fetch those attributes.
    This can lead to very wrong situations in which, for instance, the GPU present the *normal*
    attribute as having the index `4` but the shader tries to fetch them on index `2`. Vertex
    semantics fix this situation by adding an intermediate layer both the user and the GPU (i.e. via
    [luminance]) must speak: semantics. Semantics are user-defined and none is hardcoded in
    [luminance]. As soon as a type uses vertex semantics, all the type system knows how to forward
    that information to shaders and what shaders should do to fetch them. The other bonus, ultra
    cool thing is that shaders and memory buffers now compose way better: since vertex semantics
    define a sort of namespace, you will never have two shaders using the same index for different
    semantics (unless you use completely different vertex semantics type). A very cool feature I’m
    proud of and that I will detail in a future article / [luminance] tutorial.
  - And lots of other doc fixes, enhancements, etc.

# One word about the feature set and spare-time

You might be wondering *“Woah, this is such a big feature set. A lot of things are coming! Why not
having split the feature set into several releases?”*, and that’s a good question.

Lately, I’ve been feeling on-and-off about everything I do on my spare-time on the FOSS level. I
have several crates I care about and sometimes people show up and start asking for features and/or
bug fixes. This takes some time and after a long day of work, I sometimes feel that I just want to
hack on things I like, play my electric guitar, go to swim or wakeboard or just spend time with
people I care about and love.

Currently, [luminance] — even though it has an unexpected amount of downloads! — is not a famous
crate. People nowadays talk about crates living in a higher-level stack, such as [ggez], [gfx],
[amethyst], [piston], etc. I don’t communicate *a lot* about [luminance] because of, mostly, two
things:

  1. Imposter syndrom.
  1. *Avoid success at all cost*.

> I guess Haskellers will get the second point. ;)

My own philosophy about software and especially my own software is that it should have a top-tier
quality, both in terms of API design, elegance, type safety, performance and documentation. The
[front documentation of warmy](https://docs.rs/warmy) is a good example of something I continuously
seek, because good documentation helps my future myself to read the code and public interface but
also for onboarding people. Lots of projects — especially in professional industries — suffer from
*knowledge bubbles*, in which just a bunch (often only one!) of people know about `A` and `B` and
when someone joins in, they’re lost and have to ask questions. As an engineer, I always write
everything I know in whatever source-of-truth the company I’m in can offer (it can be a Confluence
server; a git repository; a wiki; whatever) because sharing knowledge and writing processes is key
in engineering — also, pro tip: I highly value that when interviewing candidates!

But the more I advance and grow up, the more things I learn and the more things I master. And as I
master new things, I tend to get used to them. And the “mental effort” it requires tends to slowly
disappear. I remember when I tried (hard!) to wrap my finger around what a *profunctor* is, or what
_free monads_ or even *natural transformations* are and how to use them. It was hard. As a friend of
mine ([\@lucasdicciocio]) perfectly
summarized it some times ago, when we learn something as a beginner, we struggle. But people who
master those concepts struggle even harder when trying to learn the next concepts at hand. And I’m
not an exception. Today, _free monads_ are something I know (even though I don’t use them; it’s a
myth! it’s a trap! :D) and I feel like I’m not legitimate to talk about them because _they feel
easy_ — and it’s normal, since I’ve learned them. But I see new things to learn, new hard things
that I know I’ll be struggling with for a while. And now it feeds the imposter syndrom. You know,
when you’re 20, you feel like you know a lot of things (and you likely do) and that you’re pretty
confident with your skill base. Today, I’m 27, and I feel like I will always be a rookie, because
there is always something I don’t know to struggle with. And I find that very exciting. Of course,
I don’t project that on others, which is soooo paradoxal: I don’t expect a newcomer to tell me what
a _fundep_ is or know lifetime elision rules in Rust. But I guess I have a very very strict way to
judge my knowledge. I still haven’t figured out if it’s a good thing or if it’s not.

I’ve been told that talking about what I do, what I think, what I know and my engineering philosophy
should help with this kind of questioning. So I decided to use that blog article to share some
thoughts about the next release of [luminance] that took me so much of my spare-time (but happily!)
and to talk a bit about myself. I know a lot of people in the Rust and Haskell community don’t know
me, but I also know lots of other folks do know me, in both communities. I hope my little vent will
give people some relief — maybe you too have a similar experience? Please share your comments on
[the Reddit thread this article is post in].

As always, stick doing awesome things and spend your spare-time wisely. Spend time with the ones you
love and do the things you hecking love too!

*Keep the vibes!*

[luminance]: https://crates.io/crates/luminance
[quaazar]: https://github.com/phaazon/quaazar
[gfx]: https://crates.io/crates/gfx
[gfx-hal]: https://crates.io/crates/gfx-hal
[spectra]: https://crates.io/crates/spectra
[ggez]: https://crates.io/crates/ggez
[amethyst]: https://crates.io/crates/amethyst
[piston]: https://crates.io/crates/piston
[\@lucasdicciocio]: http://dicioccio.fr
[ECS]: https://en.wikipedia.org/wiki/Entity_component_system
[the Reddit thread this article is post in]:
