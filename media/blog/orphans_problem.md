# Foreword

The [orphan instance problem] is a well known problem that I’ve discovered while designing code in
Haskell years ago. According to the Haskell wiki:

> An orphan instance is a type class instance for class C and type T which is neither defined in
> the module where C is defined nor in the module where T is defined. 

If it’s a bit overwhelming for you, here it is rephrased: this problem happens whenever you try to
create an instance of a typeclass `C` in a module that is not the module where `C` is defined for a
type `T` while you’re not in the module that actually introduces that `T` type. Since it’s not
possible to pick an instances by hand, GHC disables you from writing several instances for a same
combination of typeclass and types.

> GHC actually has a workaround for the problem: the `{-# OPTIONS_GHC -fno-warn-orphans #-}` pragma
> annotation (or the `-fno-warn-orphans` compiler option). However, this is not recommended if you
> can find another way because you expose yourself to ambiguities there.

Because Rust also has types and typeclasses (traits), it doesn’t escape the problem very much. You
still have orphan instances in Rust (called [Orphan Rules] – unofficial link). There’re scoped to
crates though, not modules.

In this blog entry, I want to explore a specific problem of orphans and how I decided to solve it in
a crate of mine. The problem is the following:

> **Given a crate that has a given responsibility, how can someone add an implementation of a given
> trait without having to use a type wrapper or augment the crate’s scope?**

# A harsh solution: type wrappers

The typical workaround to this problem is to use a type wrapper. It works by encapsulating the
origin type into another layer of typing so that the compiler recognize it as a complete different
type, allowing you to implement whichever traits you want. Rust has a nice feature added to that: if
you implement the `Deref` and `DerefMut` traits, you’re given access to all the implementations of
the source type via [deref rules], which is pretty sick. This works if you’re writing a binary,
because no one will ever use your type wrapper, only you (and your team mates, but you’ll never
*publish* the type so you’re breaking the orphan problem). However, what happens if you’re writing
a library?

You will expose a type that is not the source one, forcing people to wrap it again if they want to
implement another feature. If your type has a very different from the source one, it’s okay.
However, if you want to stick to the original semantics and use cases, people might get confused,
especially whenever they will write functions that accept as arguments the source type.

# The problem explained in `splines`

[`splines`] is a crate that provides you with spline interpolation. You can interpolate values in
several dimensions with several kind of interpolators (step, linear, cosine, etc.).

Clearly, the scope of [`splines`] is to provide math and curves types and functions, nothing more.
However, I use them in [`spectra`], a demoscene crate of mine, in which I need them to implement
some [`serde`] traits in order to serialize and deserialize them. Are you starting to see the
problem?

When I thought about where I should write the code to allow serialization / deserialization, I came
across the realization that writing it directly in `splines` would add another set of dependencies
for *everyone*, even people not using the serialization part of it. I was a bit unsatisfied with
that. So I thought about *“Why not just adding another crate, like `splines-serde`?”* Obviously,
this doesn’t work because of the orphan rules: you cannot write an implementation of a trait in a
different crate than where the type or the trait is defined. Meh.

# A limited solution yet a solution: feature-gated impls

So I came with an idea, that is not perfect, but fits my needs pretty well. Rust has that
interesting concept of [features], allowing for conditional compilation. `cargo` supports them in
the manifest and even allows you to declare dependencies as optional. The combination of both
features and optional dependencies is key here.

## Step 1: the manifest

Let’s have a look at the [current `Cargo.toml` manifest](https://github.com/phaazon/splines/blob/3cd65dce54510e289e85a1a680a60f06abff5f73/Cargo.toml)
in order to get how it’s done – I’ll slice the upper part and only show the features-related part:

```toml
[features]
default = ["std", "impl-cgmath"]
serialization = ["serde", "serde_derive"]
std = []
impl-cgmath = ["cgmath"]

[dependencies.cgmath]
version = "0.16"
optional = true

[dependencies.serde]
version = "1"
optional = true

[dependencies.serde_derive]
version = "1"
optional = true
```

Here, you can see that we are using *default features*. That means that if you depend on `splines`
in simple way (for instance, by just giving a version string, i.e. `splines = "0.2"`), you’ll get
those features enabled by default. In the `splines` case, you’ll get the `"std" ` and
`"impl-cgmath"` features by default.

Looking at the `"std"` feature, you can see that it doesn’t depende on anything else. That feature
will just make the whole library use the `std` crate. If you disable it, you can compile `splines`
with `no_std`.

The `"impl-cgmath"` has a dependency on `cgmath` and you can see that it depends on the `"0.16"`
version and that it’s optional. What it means is that if you disable default features you will not
depend on `cgmath` anymore and thus, you will not even download / compile the dependency.

If you look closely at that manifest, you also see a feature that must be set explicitly:
`"serialization"`. That feature depends on both `serde` and `serde_derive`, adding support for the
serialization code we talked earlier.

All of this is great and cool but how do we write the `impl`s based on that manifest?

## Step 2: the conditional code

There’re no rules here and people will give different advices on how you should write conditional
code. I tend to remain simple as long as the project is not too complex. `splines` is a pretty
simple and small project, so let’s get things straight!

The first thing to do is to know how we can access the features in Rust code. This is easy: via
*attributes*. Attributes in Rust are enclosed in square brackets preceded by a dash and bang –
`#![]`. There’s an interesting attribute: `cfg()`. It gives access to the configuration of the
project, for short. It may take several kind of parameters but we’re interested in only one:
`feature`. The syntax is the following:

```rust
#![cfg(feature = "name_of_feature")]
```

This oneliner will evaluate the following block whenever the `"name_of_feature"` is set. If you want
to evaluate a block if a feature is not set, you can use the `not()` combinator:

```rust
#![cfg(not(feature = "name_of_feature"))]
```

It’s a bit ugly, but it does the job.

One final and cool attribute we’ll be using: `cfg_attr()`. It takes two arguments: the first one is
a regular parameter you’d give to `cfg` that gets substituted as a boolean expression (`feature(…)`,
`not(feature(…))`, etc.) and the second one is an attribute that will get set whenever the former
argument gets substituted successfully. For instance, this:

```rust
#![cfg(not(feature = "std"))]
#![no_std]
```

Can be rewritten more elegantly as this:

```rust
#![cfg_attr(not(feature = "std"), no_std)]
```

For the rest, as `#![]` applies to the direct item after it, it’s easy to write conditional
`extern crate` for instance:

```rust
// on no_std, we also need the alloc crate for Vec
#[cfg(not(feature = "std"))] extern crate alloc;

#[cfg(feature = "impl-cgmath")] extern crate cgmath;

#[cfg(feature = "serialization")] extern crate serde;
#[cfg(feature = "serialization")] #[macro_use] extern crate serde_derive;
```

The `#[cfg_attr(…)` is even nicer when wanting to insert attributes on type, as with the `Key`
type:

```rust
#[derive(Copy, Clone, Debug)]
#[cfg_attr(feature = "serialization", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "serialization", serde(rename_all = "snake_case"))]
pub struct Key<T> { … }
```

If you want *static ifs* in the actual implementation, you can cheat and use blocks as they’re
items!

```rust
Interpolation::Cosine => {
  let cp1 = &keys[i+1];
  let nt = normalize_time(t, cp0, cp1);
  let cos_nt = {
    #[cfg(feature = "std")]
    {
      (1. - f32::cos(nt * consts::PI)) * 0.5
    }

    #[cfg(not(feature = "std"))]
    {
      use core::intrinsics::cosf32;
      unsafe { (1. - cosf32(nt * consts::PI)) * 0.5 }
    }
  };

  Some(Interpolate::lerp(cp0.value, cp1.value, cos_nt))
}
```

And finally, the one we were looking for our orphans problem:

```rust
#[cfg(feature = "impl-cgmath")]
impl Interpolate for Vector2<f32> { … }
```

That `impl` will only exist if the `"impl-cgmath"` feature is set! Sweet!

I have another ultra cool use of attributes used along with existential `impl Trait`, but that’ll be
for another blog entry.

Keep the vibes!

[orphan instance problem]:  https://wiki.haskell.org/Orphan_instance#Description
[Orphan Rules]: https://github.com/Ixrec/rust-orphan-rules#what-are-the-orphan-rules
[deref rules]: https://doc.rust-lang.org/std/ops/trait.Deref.html
[`splines`]: https://crates.io/crates/splines
[`spectra`]: https://crates.io/crates/spectra
[`serde`]: https://crates.io/crates/serde
[features]: https://doc.rust-lang.org/book/first-edition/conditional-compilation.html
