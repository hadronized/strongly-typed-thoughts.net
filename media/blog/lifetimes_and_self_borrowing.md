Lately, I’ve been playing around with [alto](https://crates.io/crates/alto) in my demoscene framework. This crate in the *replacement of [openal-rs](https://crates.io/crates/openal-rs)* as **openal-rs** has
been deprecated because *unsound*. It’s a wrapper over [OpenAL](https://www.openal.org), which enables you to play 3D sounds and gives you several physical properties and effects you can apply.

# The problem

Just to let you fully understand the problem, let me introduce a few principles from alto. As a wrapper over OpenAL, it exposes quite the same interface, but adds several safe-related types. In order to use
the API, you need three objects:

- an `Alto` object, which represents the *API object* (it holds dynamic library handles, function pointers, etc. ; we don’t need to know about that)
- a `Device` object, a regular device (a sound card, for example)
- a `Context` object, used to create audio resources, handle the audio context, etc.

There are well-defined relationships between those objects that state about their lifetimes. An `Alto` object must outlive the `Device` and the `Device` must outlive the `Context`.
Basically:

```rust
let alto = Alto::load_default(None).unwrap(); // bring the default OpenAL implementation in
let dev = alto.open(None).unwrap(); // open the default device
let ctx = dev.new_context(None).unwrap(); // create a default context with no OpenAL extension
```

As you can see here, the lifetimes are not violated, because `alto` outlives `dev` which outlives `ctx`. Let’s dig in the type and function signatures to get the lifetimes right
(documentation [here](https://docs.rs/alto/1.0.5/alto)).

```rust
fn Alto::open<'s, S: Into<Option<&'s CStr>>>(&self, spec: S) -> AltoResult<Device>
```

The `S` type is just a convenient type to select a specific implementation. We need the default one, so just pass `None`. However, have a look at the result. `AltoResult<Device>`. I told you about
lifetime relationships. This one might be tricky, but you always have to wonder “is there an elided lifetime here?”. Look at the `Device` type:

```rust
pub struct Device<'a> { /* fields omitted */ }
```

Yep! So, what’s the lifetime of the `Device` in `AltoResult<Device>`? Well, that’s simple: the lifetime elision rule in action is one of the simplest:

> If there are multiple input lifetime positions, but one of them is &self or &mut self, the lifetime of self is assigned to all elided output lifetimes.
([source](https://doc.rust-lang.org/beta/nomicon/lifetime-elision.html))

So let’s rewrite the `Alto::open` function to make it clearer:

```rust
fn Alto::open<'a, 's, S: Into<Option<&'s CStr>>>(&'a self, spec: S) -> AltoResult<Device<'a>> // exact same thing as above
```

So, what you can see here is that the `Device` must be valid for the same lifetime as the reference we pass in. Which means that `Device` cannot outlive the reference. Hence, it cannot outlive the
`Alto` object.

---

```rust
impl<'a> Device<'a> {
  // …
  fn new_context<A: Into<Option<ContextAttrs>>>(&self, attrs: A) -> AltoResult<Context>
  // …
}
```

That looks a bit similar. Let’s have a look at `Context`:

```rust
pub struct Context<'d> { /* fields omitted */ }
```

Yep, same thing! Let’s rewrite the whole thing:

```rust
impl<'a> Device<'a> {
  // …
  fn new_context<'b, A: Into<Option<ContextAttrs>>>(&'b self, attrs: A) -> AltoResult<Context<'b>>
  // …
}
```

Plus, keep in mind that `self` is actually `Device<'a>`. The first argument of this function then awaits a `&'b Device<'a>` object!

> rustc is smart enough to automatically insert the `'a: 'b` lifetime bound here – i.e. the 'a lifetime outlives 'b. Which makes sense: the reference will die before the `Device<'a>` is dropped.

Ok, ok. So, what’s the problem then?!

# The (real) problem

The snippet of code above about how to create the three objects is straight-forward (though we don’t take into account errors, but that’s another topic). However, in my demoscene framework, I really don’t
want people to use that kind of types. The framework should be completely agnostic about which technology or API is used internally. For my purposes, I just need a single type with a few methods to work
with.

Something like that:

```rust
struct Audio = {}

impl Audio {
  pub fn new<P>(track_path: P) -> Result<Self> where P: AsRef<Path> {}

  pub fn toggle(&mut self) -> bool {}

  pub fn playback_cursor(&self) -> f32 {}

  pub fn set_playback_cursor(&self, t: f32) {}
}

impl Drop for Audio {
  fn drop(&mut self) {
    // stop the music if playing; do additional audio cleanup
  }
}
```

This is a very simple interface, yet I don’t need more. `Audio::set_playback_cursor` is cool when I debug my demos in realtime by clicking a time panel to quickly jump to a part of the music.
`Audio::toggle()` enables me to pause the demo to inspect an effect in the demo. Etc.

However, how can I implement `Audio::new`?

# The (current) limits of borrowing

The problem kicks in as we need to wrap the three types – `Alto`, `Device` and `Context` – as the fields of `Audio`:

```rust
struct Audio<'a> {
  alto: Alto,
  dev: Device<'a>,
  context: Context<'a>
}
```

We have a problem if we do this. Even though the type is correct, we cannot correctly implement `Audio::new`. Let’s try:

```rust
impl<'a> Audio<'a> {
  pub fn new<P>(_: P) -> Result<Self> where P: AsRef<Path> {
    let alto = Alto::load_default(None).unwrap();
    let dev = alto.open(None).unwrap();
    let ctx = dev.new_context(None).unwrap();

    Ok(Audio {
      alto: alto,
      dev: dev,
      ctx: ctx
    })
  }
}
```

As you can see, that cannot work:

```
error: `alto` does not live long enough
  --> /tmp/alto/src/main.rs:14:15
   |
14 |     let dev = alto.open(None).unwrap();
   |               ^^^^ does not live long enough
...
22 |   }
   |   - borrowed value only lives until here
   |
note: borrowed value must be valid for the lifetime 'a as defined on the body at 12:19...
  --> /tmp/alto/src/main.rs:12:20
   |
12 |   fn new() -> Self {
   |                    ^

error: `dev` does not live long enough
  --> /tmp/alto/src/main.rs:15:15
   |
15 |     let ctx = dev.new_context(None).unwrap();
   |               ^^^ does not live long enough
...
22 |   }
   |   - borrowed value only lives until here
   |
note: borrowed value must be valid for the lifetime 'a as defined on the body at 12:19...
  --> /tmp/alto/src/main.rs:12:20
   |
12 |   fn new() -> Self {
   |                    ^

error: aborting due to 2 previous errors
```

What’s going on here? Well, we’re hitting a problem called the problem of **self-borrowing**. Look at the first two lines of our implementation of `Audio::new`:

```rust
let alto = Alto::load_default(None).unwrap();
let dev = alto.open(None).unwrap();
```

As you can see, the call to `Alto::open` borrows `alto` – via a `&Alto` reference. And of course, you cannot move a value that is borrowed – that would invalidate all the references pointing to it.
We also have another problem: imagine we could do that. All those types implement `Drop`. Because they basically all have the same lifetime, there’s no way to know which one borrows information from whom.
The *dropchecker* has no way to know that. It will then refuse to code creating objects of this type, because dropping might be unsafe in that case.

# What can we do about it?

Currently, this problem is linked to the fact that the lifetime system is a bit too restrictive and doesn’t allow for **self-borrowing**. Plus, you also have the *dropchecker* issue to figure out. Even
though we were able to bring in `alto` and `device` altogether, how do you handle `context`? The *dropchecker* doesn’t know which one must be dropped first – there’s no obvious link at this stage between
`alto` and all the others anymore, because that link was made with a reference to `alto` that died – we’re moving out of the scope of the `Audio::new` function.

![](http://phaazon.net/pub/rust_self_borrowing.jpg)

That’s a bit tough. The current solution I implemented to fix the issue is ok–ish, but I dislike it because it adds a significant performance overhead: I just moved the initialization code in a thread that
stays awake until the `Audio` object dies, and I use a synchronized channel to communicate with the objects in that thread. That works because the thread provides us with a stack, that is the support of
lifetimes – think of scopes.

Another solution would be to move that initialization code in a function that would accept a closure – your application. Once everything is initialized, the closure is called with a few callbacks to
toggle / set the cursor of the object living “behind” on the stack. I don’t like that solution because it modifies the main design – having an `Audio` object was the goal.

Other solutions are:

- `std::mem::transmute` to remove the lifetimes (replace them with `'static`). That’s **hyper dangerous** and we are just breaking Rust’s lifetimes… *not okay* :(
- change our design to meet the same as alto’s (in a word: use the same three objects)
- cry deeply

I don’t have a satisfying solution yet to that problem. My thread solution works and lets me have a single type abstracting all of that, but having a thread for such a thing is a waste of resources to me. I
think I’ll implement the closure solution as, currently, it’s not possible to embed in struct lifetimes’ semantics / logic. I guess it’s okay; I guess the problem is also linked to the fact the concept is
pretty young and we’re still kind of experimenting it. But clearly, lifetimes hit a hard problem here that they cannot solve correctly. Keep in mind that even if unsafe solutions exist, we’re talking about
a library that’s designed to work with Rust lifetimes as a pretty high level of abstraction. Firing `transmute` is very symptomatic of something wrong. I’m open to suggestions, because I’ve been thinking the
problem all day long without finding a proper solution.

Keep the vibe!
