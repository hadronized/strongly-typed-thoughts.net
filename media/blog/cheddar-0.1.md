It’s been a long time I wanted to announce and reveal this, and this morning is now the moment!
This blog post announces the release of [cheddar] in version 0.1!

# Foreword

Cheddar is a GLSL *superset* language. What it means is that most of the GLSL constructs and syntax
you’re used to is valid in Cheddar – not *all of it*; *most of it*. Cheddar adds a set of features
that I think are lacking to GLSL. Among them:

  - Some non-valid GLSL constructions made valid in Cheddar to ease the writing of certain shader
    stages.
  - A more functional approach to programming shaders on the GPU.
  - Structures, types and GLSL-specific *constructs sharing*.
  - Imports and modules with live reloading and transitive dependencies.

However, Cheddar is not:

  - A new shading language that abstracts over any kind of shading language (i.e. unlike [Cg] for
    instance). However, since it should be possible for GLSL to be transpiled to SPIR-V (hence
    Vulkan), it should be possible to transpile Cheddar to SPIR-V.
  - A dedicated DSL to a specific graphics problem. Cheddar is very similar to GLS (actually, that’s
    why it’s called a GLSL *superset*). If you’re looking for a solution to ease writing shaders in
    a way that you don’t have to learn shaders, Cheddar won’t help you much with this.

The documentation is still a big *work in progress* but most of it should give you enough
information to get started.

# A bit of backstory

Cheddar was imagined and designed while I was working on [spectra], a *work in progress* demoscene
crate I’ve been working for a while now. I released two demos thanks to [spectra] –
[this one](https://www.youtube.com/watch?v=pYqZS1C_7PU) and
[this one](https://www.youtube.com/watch?v=ug7eRowgVw4). Because those demos are very *meh* to me, I
decided to enhance my tools. Among them was the need to write shaders a better and easier way. Then
Cheddar got born. I used Cheddar while preparing other demos, and I was talking about it on IRC and
Reddit, people seemed to be interested – I even had a friend writing demos in C++ interested!

So here it is. Please, provide feedback if you try it, should you like it or not!

# Dig in

> Disclaimer: the current state of the language is pretty experimental and unstable. There’s no
> semantics checking, for instance.

You can read the full tutorial and design document on the [official documentation page].

Thanks for having read me, and as always, keep the vibe!


[cheddar]: https://crates.io/crates/cheddar
[Cg]: https://en.wikipedia.org/wiki/Cg_(programming_language)
[spectra]: https://crates.io/crates/spectra
[official documentation page]: https://docs.rs/cheddar/0.1.0/cheddar
