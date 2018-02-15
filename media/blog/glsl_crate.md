It’s been almost two months that I’ve been working on my [glsl](https://crates.io/crates/glsl) crate. This crate
exposes a GLSL450 compiler that enables you to parse GLSL-formatted sources into memory in the form of an
AST. Currently, that AST is everything you get from the parsing process – you’re free to do whatever you want
with it. In the next days, I’ll write a GLSL writer (so that I can check that I can parse GLSL to GLSL…). I’d
love to see contributions from Vulkan people to write a SPIR-V backend, though!

Just for the record, the initial goal I had in mind was to parse a subset of GLSL for my
[spectra](https://github.com/phaazon/spectra) demoscene framework. I’ve been planning to write my own GLSL-based
shading language (with modules, composability, etc. etc.) and hence I need to be able to parse GLSL sources.
Because I wanted to share my effort, I decided to create a dedicated project and here we are with a GLSL crate.

Currently, you can successfully parse a GLSL450-formatted source (or part of it, I expose all the intermediary
parsers as well as it’s required by my needs to create another shading language over GLSL). See for instance
[this shading snippet](https://gist.github.com/phaazon/fbe7a9c26bdea4a7262d2ea028c578ce) parsed to an AST.

However, because the project is still very young, there a lot of features that are missing:

- I followed the [official GLSL450 specifications](https://www.khronos.org/registry/OpenGL/specs/gl/GLSLangSpec.4.50.pdf),
  which is great, but the types are not very intuitive – some refactoring must be done here;
- I use [nom](https://crates.io/crates/nom) as a parser library. It’s not perfect but it does its job very well.
  However, error reporting is pretty absent right now (if you have an error in your source, you’re
  basically left with `Error(SomeVariant)` flag, which is completely unusable;
- I wrote about 110 unit tests to ensure the parsing process is correct. However, there’s for now zero semantic
  check. Those are lacking.
- About the semantic checks, the crate is also missing a semantic analysis. I don’t really know what to do here,
  because it’s a lot of work (like, ensure that the assigned value to a float value is a real float and not a
  boolean or ensure that a function that must returns a vec3 returns a vec3 and not something else, etc.). This
  is not a trivial task and because this is already done by the OpenGL drivers, I won’t provide this feature
  yet. However, to me, it’d be a great value.

If you’re curious, you can start using the crate with `glsl = "0.2.1"` in your `Cargo.toml`. You probably want
to use the `translation_unit` parser, which is the most external parser (it parses an actual shader). If you’re
looking for something similar to my need and want to parse subparts of GLSL, feel free to dig in the
[documentation](https://docs.rs/glsl), and especially the `parsers` module, that exports all the parsers
available to parse GLSL’s parts.

Either way, you have to pass a bytes slice. If your source’s type is `String` or `&str`, you can use the 
`str::as_bytes()` function to feed the input of the parsers.

> Not all parsers are exported, only the most important ones. You will not find an octal parser, for instance,
> while it’s defined and used in the crate internally.

# Call to contribution

If you think it’s worth it, I’m looking for people who would like to:

- write a GLSL to SPIR-V writer: it’s an easy task, because you just have to write a function of the
  form `AST -> Result<SPIRV, _>`, for being pragmatic;
- test the crate and provide feedback about any error you’d find! Please do not open an issue if you have
  an error in your source and find *“the error from the parsers is not useful”*, because it’s already
  well established this is a problem and I’m doing my best to solve it;
- any kind of contributions you think could be interesting for the crate.

Happy coding, and keep the vibe!
