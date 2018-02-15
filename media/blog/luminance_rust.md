I wanted to write that new article to discuss about something important I’ve been doing for several 
weeks. It’s actually been *a month* that I’ve been working on *luminance*, but not in the usual
way. Yeah, I’ve put my *Haskell* experience aside to… port *luminance* into *Rust*! There are
numerous reasons why I decided to jump in and I think it could be interesting for people to know
about the differences I’ve been facing while porting the graphics library.

# You said Rust?

Yeah, [Rust](https://www.rust-lang.org). It’s a strong and static language aiming at system
programming. Although it’s an *imperative* language, it has interesting *functional* conventions
that caught my attention. Because I’m a *haskeller* and because *Rust* takes **a lot** from
*Haskell*, learning it was a piece of cake, even though there are a few concepts I needed a few days
to wrap my mind around. Having a strong C++11/14 experience, it wasn’t that hard though.

## How does it compare to Haskell?

The first thing that amazed me is the fact that it’s actually not that different from *Haskell*!
*Rust* has a powerful type system – not as good as *Haskell*’s but still – and uses immutability as
a default semantic for bindings, which is great. For instance, the following is forbidden in *Rust*
and would make `rustc` – the *Rust* compiler – freak out:

```rust
let a = "foo";
a = "bar"; // wrong; forbidden
```

*Haskell* works like that as well. However, you can introduce mutation with the `mut` keyword:

```rust
let mut a = "foo";
a = "bar"; // ok
```

Mutation should be used only when needed. In *Haskell*, we have the `ST` monad, used to introduce
local mutation, or more drastically the `IO` monad. Under the wood, those two monads are actually
almost the same type – with different warranties though.

*Rust* is strict by default while *Haskell* is lazy. That means that *Rust* doesn’t know the concept
of *memory suspensions*, or *thunks* – even though you can create them by hand if you want to. Thus,
some algorithms are easier to implement in *Haskell* thanks to laziness, but some others will
destroy your memory if you’re not careful enough – that’s a very common problem in *Haskell* due to
thunks piling up in your stack / heap / whatever as you do extensive lazy computations. While it’s
possible to remove those thunks by optimizing a *Haskell* program – profiling, strictness, etc.,
*Rust* doesn’t have that problem because it gives you full access to the memory. And that’s a good
thing **if you need it**. *Rust* exposes **a lot** of primitives to work with memory. In contrast
with *Haskell*, it doesn’t have a *garbage collector*, so you have to handle memory on your own. Well,
not really. *Rust* has several very interesting concepts to handle memory in a very nice way. For
instance, objects’ memory is held by *scopes* – which have *lifetimes*.
[RAII](https://en.wikipedia.org/wiki/Resource_Acquisition_Is_Initialization) is a very well known
use of that concept and is important in *Rust*. You can glue code to your type that will be ran when
an instance of that type dies, so that you can clean up memory and scarce resources.

*Rust* has the concept of *lifetimes*, used to give names to scopes and specify how long an object
reference should live. This is very powerful yet a bit complex to understand in the first place.

I won’t go into comparing the two languages because it would require several articles and a lot of
spare time I don’t really have. I’ll stick to what I’d like to tell you: the *Rust* implementation
of *luminance*.

# Porting luminance from Haskell to Rust

The first very interesting aspect of that port is the fact that it originated from a realization
while refactoring some of my *luminance* *Haskell* code. Although it’s functional, stateless and
type-safe, a typical use of *luminance* doesn’t *really* require laziness nor a garbage collector.
And I don’t like using a tool – read language – like a bazooka. *Haskell* is the most powerful
language ever in terms of abstraction and expressivity over speed ratio, but all of that power comes
with an overhead. Even though you’ll find folks around stating that *Haskell* is pretty okay to code
a video game, I think it will never compete with languages that are **made** to solve real time
computations or reactive programming. And don’t get me wrong: I’m sure you can write a decent video
game in *Haskell* – I qualify myself as a *Haskeller* and I’ve not been writing *luminance* just for
the joy of writing it. However, the way I use *Haskell* with *luminance* shouldn’t require all the
overhead – and profiling got me right, almost no GC was involved.

So… I looked into *Rust* and discovered and learned the language in only three days. I think it’s due
to the fact that *Rust*, which is simpler than *Haskell* in terms of type system features and has
almost everything taken from *Haskell*, is, to me, an **imperative Haskell**. It’s like having a
*Haskell* minus a few abstraction tools – HKT (but they’ll come soon), GADTs, fundeps, kinds,
constraints, etc. – plus a total control of what’s happening. And I like that. A lot. A *fucking*
lot.

Porting *luminance* to *Rust* wasn’t hard as a *Haskell* codebase might map almost directly to
*Rust*. I had to change a few things – for instance, *Rust* doesn’t have the concept of existential
quantification as-is, which is used intensively in the *Haskell* version of *luminance*. But most
*Haskell* modules map directly to their respective *Rust* modules. I changed the architecture of
the files to have something clearer. I was working on *loose coupling* in *Haskell* for *luminance*.
So I decided to directly introduce loose coupling into the *Rust* version. And it works like a
charm.

So there are, currently, two packages available: `luminance`, which is the core API, exporting the
whole general interface, and `luminance-gl`, an **OpenGL 3.3** backend – though it will contain more
backends as the development goes on. The idea is that you need both the dependencies to have access
to *luminance*’s features.

I won’t say much today because I’m working on a [demoscene](https://en.wikipedia.org/wiki/Demoscene)
production using *luminance*. I want it to be a proof that the framework is usable, works and acts
as a first true example. Of course, the code will be open-source.

The documentation is not complete yet but I put some effort documenting almost everything. You’ll
find both the packages here:

[luminance-0.1.0](https://crates.io/crates/luminance)

[luminance-gl-0.1.0](https://crates.io/crates/luminance-gl)

I’ll write another article on how to use *luminance* as soon as possible!

Keep the vibe!
