A year ago, [I announced the glsl crate](https://phaazon.net/blog/glsl_crate), a crate used to parse
GLSL450-formated sources (and possibly GLSL460 since it’s almost the same thing, but a feature flag
is needed for that). I didn’t get much feedback about it but I’ve been using it in my [cheddar]
crate with a great of success (along with [spectra]).

The [glsl] crate enables you to parse a GLSL-formatted input (raw string or byte slice) into an AST
you can manipulate in Rust.

It’s very likely you will never have to work with that AST representation directly. Instead, you
might be interested in all the possible outcomes you can get from using the [glsl] crate:

  - Parse a file, like a `scene.glsl`, and get its AST representation in Rust.
  - Pass that AST to a *writer*, for instance, a [transpiler], to convert from the GLSL to [SPIR-V]
    or [HLSL].
  - Algorithmically enhance or enrich the GLSL AST without having to cope with raw strings.
  - Extract information from the GLSL source code (reflection).
  - Etc.

Lately, in [spectra], I’ve been working on a more abstract way to write shaders in GLSL. The idea is
that I need to ship the library with some GLSL. The typical way people do this is by embedding a
static string into your library / application, like so:

```
const SOME_VERTEX_SHADER: &str =
"void main() {
  // …
}";
```

> You can also use the `include_str!` standard macro to read it from a file at compile-time.

The [cheddar] crate provides you with an enriched version of GLSL (the Cheddar language), and the
integration in [spectra] is pretty much seamless. The GLSL is parsed from Cheddar, transpiled to the
GLSL AST and then the whole thing is passed [luminance] that acts as a rendering backend of
[spectra]. Becauses some AST transformation is needed, the [glsl] crate is used in [cheddar], and
some GLSL must be added in order to provide a smoother and nicer experience within [spectra].

The problem kicks in that transformation part. Since Cheddar is parsed directly into memory as an
AST (via the [cheddar] crate), how could we transform and add things to values from [cheddar]?

# The hard and manual way: the constructor syntax

Pretty much all of the [glsl] crate is public. There’s no invariant, so no need to hide anything.
It’s possible to create, for instance, a `AssignmentExpr` by simply calling its constructor syntax:

```
let assignment_expr = AssignmentExpr {
  // fields here
  };
```

This is the regular way to go, but if you look at some types, such as the
[`SimpleStatement`](https://docs.rs/glsl/0.9.2/glsl/syntax/enum.SimpleStatement.html), you’ll get
that it can get very noisy and verbose very quickly. So that’ll work, but that’ll also be very
incomfortable to work with.

The advantage of constructing and transforming that way is that you can destructure and
pattern-match ASTs.

# Enter the nice solution: quasiquoting

As a Haskeller, I’ve been very frustrated by that construction problem, thinking that there must be
an easier and less noisy way to do so. That reminds me of all the EDSLs I wrote in my Haskeller life
and some very, *very* sweet embedded languages, like SQL, C or Java, directly into Haskell. That is
done via something we call [quasiquoting]. Quasiquoting is a general concept but in our case,
applies to embedded languages.

The concept is simple: you write in a foreign language in a *quasiquoter* in your host language
(Haskell or Rust). The compiler then recognizes the quasiquoter and **executes some code at compile
time**. Once it’s done, it replaces the quasiquoted statement with the result of its computation.

One good example of quasiquoting is [postgresql-query], a Haskell quasiquoting library for SQL. SQL
queries can be built with regular Haskell code but they can also be built out of a *quasiquoter*,
like so:

```
-- some code elided for simplicity here
pqQuery [sqlExp|SELECT u.id, u.name, u.age
                FROM users AS u ^{cond}
                ORDER BY ^{ord}|]
```

The `[sqlExp|…|]` notation uses the quasiquoter syntax: the `sqlExp` is the quasiquoter to use, and
everything between the two pipes are the content / input to pass the quasiquoter. The compiler just
reads that, executes some code defined in the [postgresql-query] library and if it runs correctly,
the resulting expression is inserted directly at the place of use as if nothing has happened.
Brillant, right? :)

Having written [glsl], that seemed like a perfect – maybe even wanted and mandatory? – opportunity
to me to introduce GLSL quasiquoting into the Rust environment. Maybe it’ll also bring some people
onto the [glsl] project – I really need hands, especially because the [SPIR-V] transpiler doesn’t
exist yet!

So, here you go. [glsl-quasiquote-0.1] was released today! The crate provides you with two macros:
`glsl!` and `glsl_str!`. Both are procedural macros that requires a nightly compiler and the
`proc_macro_non_items` feature. They will both output a [`TranslationUnit`], that represents a whole
shader AST. 

The `glsl!` AST works the same way a quasiquoter from Haskell: instead of delimiting your GLSL code
with pipes, you write them inside parens or brackets, like so:

```
glsl!{
  void main() {
  }
}
```

The `glsl_str!` macro does the exact same thing but expects a literal string as input. This is due
to the fact that quasiquoting in Rust is a bit tricky – who’s looking at me like I was about to
write an RFC? – because regular macros and procedural macros eat Rust tokens as inputs, forcing
their content to be Rust-friendly syntax. Well, GLSL is not: the `#version` and `#extension` pragmas
need newlines at the end, so the following will not work:

```
glsl!{
  #version 330 core
  void main() {
  }
}
```

Instead, you must use the `glsl_str!` quasiquoter:

```
glsl_str!{"
  #version 330 core

  void main() {
  }
"}
```

It’s a bit of an unaesthetica and pretty weird syntax but we don’t really have a choice (so far).

Whenever a GLSL is incorrectly formatted, you’ll get a compiler error. For instance, the following:

```
glsl!{
  void main() 3 {
  }
}
```

generates the following `rustc` output:

```
error: proc macro panicked
  --> tests/lib.rs:26:11
   |
26 |     let _ = glsl!{
   |  ___________^
27 | |     void main() 3 {
28 | |     }
29 | |   };
   | |___^
   |
   = help: message: GLSL error: Err(ParseError { kind: Many1, info: "void main (  ) 3 {  }" })

error: aborting due to previous error
```

The crate was released today, so it needs a lot of love, attention and testing. I’m adding support
for it in [spectra] for my experiences, so I should be updating it if I find anything wrong with it.

Feel free to use it, and have fun! I’m awaiting your feedback!

Keep the vibes!

[cheddar]: https://crates.io/crates/cheddar
[spectra]: https://crates.io/crates/spectra
[glsl]: https://crates.io/crates/glsl
[transpiler]: https://en.wikipedia.org/wiki/Source-to-source_compiler
[SPIR-V]: https://www.khronos.org/spir
[HLSL]: https://docs.microsoft.com/en-us/windows/desktop/direct3dhlsl/dx-graphics-hlsl
[luminance]: https://crates.io/crates/luminance
[quasiquoting]: https://wiki.haskell.org/Quasiquotation
[postgresql-query]: http://hackage.haskell.org/package/postgresql-query
[glsl-quasiquote-0.1]: https://crates.io/crates/glsl-quasiquote/0.1.0
[`TranslationUnit`]: https://docs.rs/glsl/0.9.2/glsl/syntax/type.TranslationUnit.html
