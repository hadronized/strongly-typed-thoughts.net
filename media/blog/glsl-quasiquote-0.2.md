[glsl-quasiquote-0.2] was released early this morning. This new version provides a more stable
public API. Two major changes:

  - The `glsl_str!` proc-macro would have only survived the `0.1` version. It’s now deprecated and
    will be removed soon.
  - The `glsl!` proc-macro now supports GLSL pragmas (both `#version` and `#extension`).

> *“Wait. GLSL pragmas? You [said it wasn’t possible](https://phaazon.net/blog/glsl-quasiquoting#enter-the-nice-solution-quasiquoting)
> because of how the Rust tokenizer thinks newlines are meaningless while the GLSL grammar uses them
> to separate pragmas from other pragmas and code.”*

Yes, at the time of writing `glsl-quasiquote`, I just thought that since [`TokenStream`] is about a
stream of Rust tokens, GLSL’s pragmas would be impossible to encode with this mechanism. In fact,
it’s actually impossible per-se: newlines are not Rust tokens and are just completely ignored by the
tokenizer… or are they?

# Enter the motivation

At the announcement of the release of `glsl-quasiquote` on Reddit, someone – actually,
[somebodddy](https://www.reddit.com/r/rust/comments/9lo9s4/glsl_quasiquoting_in_rust/e78nf85) just
pointed out that I could use Rust attributes to encode GLSL pragmas. That would enable me to have
them as regular Rust tokens at the cost of modifying very, very briefly the GLSL grammar for the
pragmas – nothing bad, really. Their idea, which I liked, was this:

Instead of:

```
#version 330 core
```

We could write:

```
#![version 300 core]
```

Such a small change yet effective, right? I was appealed by the idea, so I implemented it. The code
was a bit messy because I did all the token pattern-matching by hand but I sketched something up
that worked in a few minutes. After having a discussion with a friend on IRC (antoyo!), I realized
that I was plain wrong from the beginning assertion that newlines are ignored by the Rust tokenizer:
they’re not.

# Enter the hack

The [`proc_macro`] crate, which is used to manipulate Rust tokens, has several items that I used
to implement the quasiquoter:

  - `TokenStream`, a stream of tokens. This is a very opaque type, you can just create an empty one,
    display it as a `String` and marshall to and from an iterator interface, which element type is
    `TokenTree`.
  - `TokenTree`, a single token or a delimited sequence of token trees, like a group of tokens
    logically united (with brackets, parens, etc.).
  - All the types used in `TokenTree` variants, such as `Literal`, `Punct`, `Group`, etc.

There’s something I just had completely forgotten when claiming that newlines were ignored: pretty
much all of the tokens contained in `TokenTree` have an associated [`Span`]. I’m not sure what the
initial intent was about with this type, but in order to use it, I had to add the `proc_macro_span`
feature. That type gives positional information along with macro expansion on a token. Specifically,
it has a method that gives the line and column at which the token starts and ends.

This information gives us the line on which a token lay. That’s it. We have the newlines! A newline
is just whenever a token following another token has a different line in its span. That’s as easy as
it gets.

## The real fix

So instead of implementing GLSL pragmas via Rust attributes, I decided to implement them the way
they are defined in the spec and the [glsl] crate: plain GLSL. The idea was just to match a `#` and
accumulate tokens in a collection as long as tokens lay on the same line. As soon as the line
changes, we have the full pragma and we can call `TokenStream::from_iter`.

The current implementation expects the pragmas to be at the top part of the quasiquoted GLSL code.
You shouldn’t be trying to put the `#version` at the bottom of your file, but I wanted you to be
noticed: don’t do that!

Since [glsl-quasiquote-0.2], this Rust code now completely compiles and generates, at compile-time,
a GLSL AST:

```
let ast = glsl!{
  #version 330 core
  #extension GL_ARB_separate_shader_objects : require

  void main() {
  }
};
```

# Future announcements about glsl and glsl-quasiquote 

Some future announcements to come (things that I’ve already been working on and that I might release
soon):

  - The quasiquoter will have interpolation at some time.
  - [glsl] is about to be released with an experimental SPIR-V transpiler. The current
    implementation uses [shaderc] as backend in order to test how everything goes.

[glsl-quasiquote-0.2]: https://crates.io/crates/glsl-quasiquote/0.2.0
[`TokenStream`]: https://doc.rust-lang.org/proc_macro/struct.TokenStream.html
[`proc_macro`]: https://doc.rust-lang.org/proc_macro/index.html
[`Span`]: https://doc.rust-lang.org/proc_macro/struct.Span.html
[glsl]: https://crates.io/crates/glsl
[shaderc]: https://crates.io/crates/shaderc
