[luminance] was released in [version 0.25.5] two weeks ago and I realized I haven’t blogged about it
while it’s an important release!

> Actually, it’s from 0.25.2, but I’ll just talk about the latest patched.

That new release received several patches:

- The `gl` dependencies was updated to [gl-0.10.0].
- A new `uniform_interface!` macro was added.

I’ll introce that `uniform_interface!` macro in this blog entry.

# Auto derive <strike>free</strike> *almost free*

That macro was written to provide a smoother and better experience when writing types that will act
as *uniform interfaces* in shader. For the record, when you create a `Program<_, _, _>` with
[luminance], the third type variable is called the *uniform interface*. You’ll be handed back a
value of that type when your shader is being used. That will give you the possibility to send data
to the sader prior to making any rendering.

The main idea is to have a type like this:

```
struct Common {
  resolution: Uniform<[f32; 2]>,
  time: Uniform<f32>,
  jitter: Uniform<f32>
}
```

If you build a program which type is `Program<_, _, Common>`, whenever you ask a pipeline to use
that program, you’ll be handed a `Common` object so that you can access back the *uniforms*.

In order for that to happen, though, there’s a constraint: your type must implement a trait. That
trait is [`UniformInterface`]. There’s no magic behind that – though, it can be a bit harsh to
implement that trait. If you’re using [luminance] or plan to, I strongly advise you to try an
`impl` that trait by hand first.

Anyway, after the fifth or sixth *uniform interface* you’ll have written, you’ll start to get bored
of writing the same code every now and then. That’s the exact same feeling as for *serializing* and
*deserializing* with [serde]: it’s interesting to do it by hand the first time, but it gets really,
**really** annoying after a while.

So it’d be great to be able to automatically *derive* `UniformInterface` for your own types.
There’re two solutions here:

1. Use [procedural macros] so that we can write something like `#[derive(UniformInterface)]`.
2. Use standard `macro_rules!`.

(1) is great because it seems *seamless* when you use it – you already use
`#[derive(Clone, Debug, Eq, PartialEq, Etc)]`. However, even though it’s doable to learn [syn] in
order to parse the Rust token stream, it’ll require some time and patience while I wanted something
quick to mockup the idea. For that, (2) is perfect, because regular macros are *easy* and if
correctly written, shouldn’t add too much weirdness over the type.

> The idea is to move to (1) if I find (2) to be a success and really useful.

# The macro

So… we’re talking about the [`uniform_interface!`] macro. This macro has already a pretty decent
documentation, so feel free to visit it, but I’ll explain more here.

A macro is generally used to introduce an [EDSL]. For instance, [nom]’s [`do_parse`] macro
uses a funny EDSL (you use operators like `>>` and return result with `(…)`, which is not
*normal* Rust code).

The [`uniform_interface!`] macro uses an EDSL that you know very well: it’s plain Rust code! Hot
news: you’ll have nothing more to learn!

> Note: it’s not really *any* Rust code, since you can only define `struct`s. Then, see that as a
> subset of Rust.

The syntax is very simple. Let’s take back our sample from above with the `Common` interface but now
let’s the macro do all the magic for us!

```
#[macro_use]
extern crate luminance;

uniform_interface! {
  struct Common {
    resolution: [f32; 2],
    time: f32,
    jitter: f32
  }
}
```

You’ll notice two interesting properties:

- You define your fields without annotating them with `Uniform<_>`: the macro does it for you.
- The `UniformInterface` `impl` is automatically generated for you!

The second point is the most interesting one, since it’s akin to writing
`#[derive(UniformInterface)]`. As you can see, the syntax overhead is not really a problem.

The cool part of that macro is that it also supports Rust annotations on its fields. You have two
possible annotations available:

- `as("something")` behaves by not using the field’s direct name and instead use the one provided as
  argument.
- `unbound` enables not to make the whole *uniform interface* fail if a field cannot be mapped on
  the GPU side.

The `as(…)` annotation is very simple and straight-forward to get, since it lets you change the
binding name of the field. The `unbound` annotation is a bit trickier and you need to understand how
`UniformInterface` expects the value to be constructed.

By default, when you try to map a field to something on the GPU side (shader program), you use the
[`UniformBuilder::ask`] function. If you have a closer look at function, you can see that it returns
`Result<Uniform<T>, UniformWarning>`. In order to get the `Uniform<T>` out, you’re left with the
choice of how you should handle errors. And that will depend on how you want your value to be built.
For instance, some fields might be completely mandatory for your shader to work and others optional.
`unbound` tags a field as optional.

You can mix both annotations if you want to remap an optional field!

```
#[macro_use]
extern crate luminance;

uniform_interface! {
  // we want to export that, so we also make it pub
  pub struct Common {
    resolution: [f32; 2], // this is really required
    #[as("t")]
    time: f32, // will be referenced with "t" in the shader and is mandatory as well
    #[unbound, as("bias")]
    jitter: f32 // will be referenced with "bias" in the shader and is optional
  }
}
```

That’s all for today! I hope that feature will be useful for whomever uses [luminance] – I use it
extensively and I’ve been using `uniform_interface!` for several days now and it’s *really*
appreciated! :D

> I plan to add another macro to create *buffer types*, since they must meet GPU-specific alignment
> rules, who are boring to maintain without code generation.

Feel free to provide feedback on Reddit or GitHub and have fun!

[luminance]: https://crates.io/crates/luminance
[version 0.25.5]: https://crates.io/crates/luminance/0.25.5
[gl-0.10.0]: https://crates.io/crates/gl/0.10.0
[`UniformInterface`]: https://docs.rs/luminance/0.25.5/luminance/shader/program/trait.UniformInterface.html
[serde]: https://crates.io/crates/serde
[procedural macros]: https://doc.rust-lang.org/book/first-edition/procedural-macros.html
[syn]: https://crates.io/crates/syn
[`uniform_interface!`]: https://docs.rs/luminance/0.25.5/luminance/macro.uniform_interface.html
[EDSL]: https://wiki.haskell.org/Embedded_domain_specific_language
[nom]: https://crates.io/crates/nom
[`do_parse`]: https://docs.rs/nom/3.2.1/nom/macro.do_parse.html
[`UniformBuilder::ask`]: https://docs.rs/luminance/0.25.5/luminance/shader/program/struct.UniformBuilder.html#method.ask
