I’ve been working on several projects lately and writing a bit about them on my blog – especially,
specific technical problems. This very blog entry is on a different level though: it’s about our
tooling in Rust.

See, most tools in Rust are wonderful:

  - `rustc`, the official compiler, is pretty easy to use and even though it’s an advanced piece of
    software, it’s pretty unlikely you’ll have to use it directly (unless you’re doing something
    very exotic, or working on it directly).
  - `cargo`, by far the most appealing tool in the Rust ecosystem to me, is a Rust build system and
    package manager. It downloads and checks your projects’ dependencies, using https://crates.io as
    a default mirror for your crates (but you can
    [use other mirrors if you want and are brave enough](https://doc.rust-lang.org/cargo/reference/unstable.html#alternate-registries),
    even yours), (cross) compiles, runs library, binary and even documentation tests, etc.
  - `cargo` also comes with a plugin system, allowing people to write cargo *subcommands* that can
    be installed like with `cargo install cargo-tree` and run with `cargo tree`. Most common plugins
    are:
      - `bloat`.
      - `watch`.
      - `tree`.
      - `clippy`.
      - `fuzz`.
      - `expand`.
      - `outdated`.
      - List [here](https://github.com/rust-lang/cargo/wiki/Third-party-cargo-subcommands).
  - `rustup`, used to select, switch, install and update Rust compilers, toolchains, support
    cross-compilation and select targets, etc.
  - [RLS](https://github.com/rust-lang-nursery/rls).
  - Plus some additional, non-local tools such as
    - [Rust Playground](https://play.rust-lang.org).
    - [crates.io](https://crates.io), obviously.
    - The [excellent Rust documentation](https://doc.rust-lang.org/std), giving you information and
      documentation about the standard library, the procedural macro, the allocation system, etc.
    - The [docs.rs](https://docs.rs) documentation host, hosting ALL crates’ documentation for free,
      automatically and with fuzzy searching.
  - And many others.

Clearly, you can see we have lots of wonderful tools made by talented people and you can clearly
feel how easy it is to start writing a crate, test it and publish it along with its documentation.
It feels seamless. The documentation is a very important thing in any tech ecosystem for several
reasons. And this blog is going to be about doc tooling and some specific parts of Rust.

# Documentation is key to achieve good quality… but not only

Whatever the project you work on, you ~should~ **must** document your code. There are several
situations – let’s call this the **First Hypothesis**:

If you write code that is intended to be released publicly, you must document every public
symbols:

  - Structs, enums and type aliases.
  - Any public field.
  - Constants.
  - Functions and macros.
  - Traits along with their associated types / constants and methods.
  - Trait impls (think of how useful the documentation of a `impl Default` can be!).
  - Modules.

Now, if you are writing something that is private to a crate that you plan to release, think about
people who will want to contribute to help you with the crate. They will have to get to read your
code. They will have to go through the (sometimes painful) process of adapting to someone else’s
way to write code. So do them a favor: document your code.

Finally, what about things you don’t plan to release? Like, for instance, a binary? The same rule
actually applies: what happens if someone tries to contribute? What if you save the project aside
for some weeks and get back to it afterwards?

Those three paragraphs just show you that the **First Hypothesis** was wrong: there are not several
situations. Only one: as soon as you write code, whatever whom it’s aimed at, just document it.
You’ll be thanked and you might even thank yourself in a near future for doing so. Documenting your
code enables you to run `cargo doc --open` and immediately start *exploring*. Exploring is when you
need to catch up with a project’s ideas, concepts, or whenever you join a team as a new job and need
to get used to the codebase.

I want to share my experience here: most people are *bad* at this – it’s not necessarily their
faults, don’t blame them. Most teams I worked in were under high pressure, with business deadlines
to meet – I’ve been there and still am. This is more about a political discussion to have with “the
ones who give you such deadlines” but really, **developing a project doesn’t stop at writing code
and testing.** Onboarding and discoverability should be considered of a massive importance, because
it helps preventing the project from burying itself and getting too bloated for newcomers or even
long-running team members to understand *the actual heck is happening there*. When someone new joins
your team, you should help them to get accustomed to the codebase… but you also have to work. They
should have *leads* or *hints* to get their feet wet. Several solutions exist to document that in
*kind of* standardized ways:

  - Write a `README.md` in all the projects your team has. This should be mandatory. Describe what
    the project is about, how to build it as a developer, how to run it as a dev-ops. Describe the
    architecture, etc.
  - Write a `CONTRIBUTING.md` to help onboarding newcomers with your team guidelines, conventions,
    engineering processes, etc.
  - Be consistent: it’s not because a small project consists in only a single shell script that you
    shouldn’t write a `README.md`: **write it**!
  - Something that I love doing, especially at work: have some kind of a cron / task that builds the
    documentation for all your projects across the scope of your team… and have it host it all
    on a server documented in the top-level `README.md` of your team. This is a real plus: people
    will just go to that documentation link to search for things *“that has already been done by the
    team”* instead of reinventing the wheels – trust me, even the ones who wrote the features might
    have forgotten writing them already.

We’re now reaching the point of this blog entry: documentation can (should?) be used to explore what
a project is about, how to use it, what are the public variants and invariants, etc. But there is
however a problem in the current Rust ecosystem, preventing us from completely have a comfortable
exploration of a crate or set of crates. A very, very important kind of public symbol is missing
from the list of documented Rust symbols. Do you think you can figure out which one?

# The missing piece to exploring a crate

Have you ever worked with a customized crate? If you don’t know what I’m referring to, I’m thinking
about crates that can be configured with *features*. For instance, as a good example of this, I’ll
talk about a crate of mine: [splines]. The crate has several features you can set to achieve
different effects:

  - `"serialization"`: this feature will turn on the [serde] serialization code (both `Serialize`
    and `Deserialize`) for all the public types exposed by [splines].
  - `"impl-cgmath"`: turn this on to have some [cgmath]’s types be usable directly within [splines]
    by implementing the appropriate traits from [splines].
  - `"impl-nalgebra"`: same thing as `"impl-cgmath"` but for [nalgebra].
  - `"std"`: automatically enabled, you can disable it with `default-features = false` or
    `--no-default-features`. When disabled, the code behaves compiled with `no_std`.

All of this is currently documented in the `README.md` of the project and the top-level
documentation of the project (for instance, `splines-0.2.3` has [this paragraph about features and
what they do](https://docs.rs/splines/0.2.3/splines/#features-and-customization).

We have two problems here:

  - It’s hard for a newcomer to *discover* and *explore* features of [splines] (or any crate using
    features) because they’re not documented like a Rust symbol is (a function, a type, etc.), while
    they truly **are** recognized by `rustc` and `cargo`!
  - We can *“bypass”* this problem by documenting and having a list of features like I do with
    [splines], but that requires the project contributors to ensure to update the list whenever they
    add, remove or alter the features set.

You can see that the situation is not really comfortable. I looked into the list of PRs and issues
about that topic on [rust-lang/rfcs](https://github.com/rust-lang/rfcs) and didn’t find anything.
So I might start to write some ideas here and maybe, depending on the feedback, write a new RFC to
fix that problem.

# Pre-RFC: documenting and exploring crate features

In order to know what modification we can do without introducing breaking changes, we need to have
a look at how features are defined. This is done in a `Cargo.toml` manifest, such as (from
[splines]):

```
[features]
default = ["std", "impl-cgmath"]
serialization = ["serde", "serde_derive"]
std = []
impl-cgmath = ["cgmath"]
impl-nalgebra = ["nalgebra"]
```

A feature is a key-value object where the key is the name of the feature and the value is a list of
dependencies (crate names, crate cluster, a feature of another crate, etc. – more about that
[here](https://doc.rust-lang.org/cargo/reference/manifest.html#the-features-section)).

We could document the features here. After all, that’s what Haskell does with `cabal` and *flags*:

```
Flag Debug
  Description: Enable debug support
  Default:     False
```

So why not simply do the exact same in Rust within a `Cargo.toml` manifest? Like so:

```
[features]
default = ["std", "impl-cgmath"]

[features.serialization]
description = "Set to automatically derive Serialize and Deserialize from serde."
dependencies = ["serde", "serde_derive"]

[features.std]
description = "Compile with the standard library. Disable to compile with no_std."
dependencies = []

[features.impl-cgmath]
description = "Implement splines’ traits for some cgmath public types."
dependencies = ["cgmath"]

[features.impl-nalgebra]
description = "Implement splines’ traits for some nalgebra public types."
dependencies = ["nalgebra"]
```

In order to introduce this feature in a retro-compatible way, using the *“old”* syntax would just
behave the same way but without a description, making them optional.

The whole purpose of this addition would be that the rustdoc team could assume the existence of an
optional `description` field on features and present them when browsing a crate’s documentation,
enriching the exploration and discovering experiences of developers. Instead of having to look at
both `lib.rs` and `Cargo.toml` to look for features, one could just simply go on https://docs.rs,
look for the crate they want to get features information *et voila*!

## Features consequences

Some people might think of the *consequences* of features. Those are, after all, used for
conditional compilation. What that means is that part of the code might be hidden from the
documentation if it’s feature-gated and that the feature wasn’t set when generating the
documentation. An example with [splines] [here](https://docs.rs/splines/0.2.3/splines/trait.Interpolate.html#foreign-impls).
You can see a few implementors of the `Interpolate` trait (some basic types and some from [cgmath]
because the `"impl-cgmath"` feature is enabled by default) but not the potential ones for
[nalgebra], which is somehow misleading – people don’t know they can actually use [nalgebra] with
[splines]!

A workaround to this problem is to get a local copy of the documentation, generating it with the
wished features. However, this will not fix the problem of the discoverability.

A solution to this could be to simply *“ignore”* the features while generating the documentation –
and only while generating it **but annotate the documented symbols with the features.** That would
enable something like this (generated with `cargo doc --open --no-default-features` and retouched
to mock the idea up):

![](https://phaazon.net/media/uploads/documented_features.png)

If several features are required, they would just be presented as a list above the annotated item.

That’s all for me today. Please provide your feedback about that idea. If you like it, I’ll write an
RFC because I really think it’s a missing thing in the documentation world of Rust.

Keep the vibes!

[splines]: https://crates.io/crates/splines
[serde]: https://crates.io/crates/serde
[cgmath]: https://crates.io/crates/cgmath
[nalgebra]: https://crates.io/crates/nalgebra
