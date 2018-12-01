> This article serves as an announcement to a new crate: [proc-macro-faithful-display].

# Enhancing faith in displaying Rust tokens

## The problem

I’ve been around procedural macros in Rust for a few weeks now – in order to write my
[glsl-quasiquote] crate. Quickly, I discovered that most types cannot be inspected. For instance,
an [`Ident`] has no methods to get the underlying identifier (as a `&str` for instance). The only
way to do so is to allocate a `String`, for instance, via a `Display` use, as in:

```
let ident_str = format!("{}", ident);
```

Now, imagine you have a fully formatted stream of GLSL tokens flowing in.
[I’ve already blogged](/blog/glsl-quasiquote-0.2)
lately about the fact Rust has some specific rules that will prevent you to write *preprocessor
pragmas*. The only way to authorize them was to implement a hack (explained in the blog article).

Lately, I was working on some not-so-trivial glsl transformations in another project when I hit a
parse error. I was surprised, because the [glsl] crate contains more than 130 unit tests about
parsing. So I thought *“Well, okay, it’s alright, I might have forgotten a very specific case not
covered in my nom implementation. I’ll just get a small reproducible sample and use it as a
new integration test in [glsl]”*. That kind of reasoning is very important to me: if you find a
bug, first thing to do (if you plan to fix it) is to write a unit test **that fails first.**
This is very important because when the test succeeds, you have solved your problem. If someone
provides a contribution later and that the test breaks again, you will have just shielded the
project against a test regression. Seriously, I know it’s boring but: **write tests.**

So I wrote a test, a
[very simple one](https://github.com/phaazon/glsl/blob/ec7be696a53e3b7b12ff6ccf72c4fcf3465a1840/glsl/tests/missing_zero_float.rs#L7)
(this is actually an integration test, since I’ve found the error in a project using [glsl]). And…
the test passed. I was so surprised: *“So that code breaks in my crate, but succeeds in the
integration test suite?! Well, wait. That means that the parser is actually correct. The problem
might come from the way the parser is used… or the input.”*

So my second reaction was to have a look at where the parser was called. It was in a `glsl!`
procedural macro invocation – in my project, I hardcoded a *vertex shader* with [glsl-quasiquote].
And then, I came to the realization and remembered. [glsl-quasiquote], in order to parse the 
input Rust token, has to use `Display` on them, in order to yield a `String` usable by the [glsl]
parser. The parser is okay – i.e. the integration test passes – but the input string… loses
information.

If you have taken a look at the integration test just above, you should have noticed an interesting
construction:

```
void main() {
  float x = 1. * .5;
}
```

This is not a *semantically* valid vertex shader, but it should parse. And it does parse. But if
you write that vertex shader with [glsl-quasiquote], what you actually write is this:

```
let vertex_shader = glsl!{
  void main() {
    float x = 1. * .5;
  }
};
```

The Rust tokenizer will eat the tokens and the `Display` implementation will render the stream
by using spaces whenever it sees fit. It will not respect the initial whitespaces. It might yield
something like this:

```
void main ( ) { float x = 1. * . 5 ; }
```

Look closely: our `.5` GLSL floating point constant value (it equals `0.5`) has become `.` and `5`.
**rustc** has transformed it into two tokens instead of a single one. This is due to the fact that
this float format is not allowed in Rust and is interpreted the same way a method call would be.

## The solution

So, after realizing that, I just went for a documentation addition stating that numbers cannot be
expressed this way, and that you have to use a leading `0`. I really disliked writing that
documentation exception but I didn’t have any solution to that.

A few days later, I was thinking about [glsl] variable interpolation and got a mind click. In order
to implement preprocessor pragmas, I used a trick with [`Span`], that gave me positional information
of tokens. And I came to the realization that I could use the same kind of trick to implement a
function that would yield an object implementing `Display` by respecting the initial
layout / formatting!

Basically, a trait – [`FaithfulDisplay`] – is implemented for all flavours of Rust tokens:

  - [`Ident`].
  - [`Literal`].
  - [`Punct`].
  - [`Group`].
  - [`TokenTree`].
  - [`TokenStream`].

All the implementations carefuly carry around the previous [`Span`] in order to *pad* enough spaces
and newlines before they get formatted into the `Formatter`. This allows to reconstruct the input
layout.

All this code is packaged in the [proc-macro-faithful-display] crate. You can find the
documentation [here](https://docs.rs/proc-macro-faithful-display). I use it – and tested against –
the crate with [glsl-quasiquote]. It especially allowed me to remove all the preprocessor trick
because now newlines are taken into account.

## Is this viable?

Honestly, I have no idea. Currently, at the time of writing, it works like a charm. If you find any
bug or weird behavior, please [open an issue here](https://github.com/phaazon/glsl/issues).
However, [`proc-macro`] hasn’t yet been stabilized and the [`Span`] type is accessible through a
*feature gate*. All of this might change. However, I strongly think that:

  - Either Rust will keep that [`Span`] type because it is really helpful (its first purpose was to    provide nice error reporting).
  - Either they’ll ditch it, but since quasiquoting is important and wanted (have a look at
    [\@bodil]’s awesome [typed-html] crate for instance), something to replace it might come next.

Keep the vibes!

[proc-macro-faithful-display]: https://crates.io/crates/proc-macro-faithful-display
[glsl]: https://crates.io/crates/glsl
[glsl-quasiquote]: https://crates.io/crates/glsl-quasiquote
[`Ident`]: https://doc.rust-lang.org/stable/proc_macro/struct.Ident.html
[`Literal`]: https://doc.rust-lang.org/stable/proc_macro/struct.Literal.html
[`Punct`]: https://doc.rust-lang.org/stable/proc_macro/struct.Punct.html
[`Group`]: https://doc.rust-lang.org/stable/proc_macro/struct.Group.html
[`TokenTree`]: https://doc.rust-lang.org/stable/proc_macro/struct.TokenTree.html
[`TokenStream`]: https://doc.rust-lang.org/stable/proc_macro/struct.TokenStream.html
[`Span`]: https://doc.rust-lang.org/stable/proc_macro/struct.Span.html
[`FaithfulDisplay`]: https://docs.rs/proc-macro-faithful-display/0.1.0/proc_macro_faithful_display/trait.FaithfulDisplay.html
[\@bodil]: https://github.com/bodil
[typed-html]: https://github.com/bodil/typed-html
[`proc-macro`]: https://doc.rust-lang.org/stable/proc_macro

