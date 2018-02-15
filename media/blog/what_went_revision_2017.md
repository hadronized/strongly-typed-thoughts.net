On the weekend of 14th – 17th of April 2017, I was attending for the forth time the
[easter demoparty Revision 2017](https://2017.revision-party.net). This demoparty is the biggest so
far in the world and gathers around a thousand people coming from around the world. If you’re a
demomaker, a demoscene passionated or curious about it, that’s the party to go. It hosts plenty of
competitions, among *photos*, *modern graphics*, *oldschool graphics*, *games*, *size-limited demos*
(what we call *intros*), *demos*, *tracked and streamed music*, etc. It’s massive.

So, as always, once a year, I attend Revision. But this year, it was a bit different for me. Revision
is *very* impressive and most of the *“big demogroups”* release their productions they’ve been
working on for months or even years. I tend to think *“If I release something here, I’ll just be
kind of muted by all those massive productions.”* Well, less than two weeks before Revision 2017, I
was contacted by another demogroup. They asked me to write an *invitro* – a kind of *intro* or
*demo* acting as a communication production to invite people to go to another party. In my case, I
was proposed to make the [Outline 2017](http://outlinedemoparty.nl) invitro. Ouline was the first
party I attended years back then, so I immediately accepted and started to work on something. That
was something like 12 days before the Revision deadline.

I have to tell. It was a challenge. All productions I wrote before was made in about a month and a
half and featured less content than the Outline Invitation. I had to write a lot of code from
scratch. *A lot*. But it was also a very nice project to test my demoscene framework, written in
Rust – you can find [spectra here](https://github.com/phaazon/spectra) for now; it’s unreleased by
the time I’m writing this but I plan to push it onto crates.io very soon.

An hour before hitting the deadline, the beam team told me their Ubuntu compo machine died and that
it would be neat if I could port the demo to Windows. I rushed like a fool to make a port – I even
forked and modified my OpenAL dependency! – and I did it in 35 minutes. I’m still a bit surprised
yet proud!

Anyways, this post is not about bragging. It’s about hindsight. I did that for
[Céleri Rémoulade](http://www.pouet.net/prod.php?which=67966) as I was the only one working on it –
music, gfx, direction and obviously the Rust code. I want to draw a list of *what went wrong* and
*what went right*. In the first time, for me. So that I have enhancement axis for the next set of
demos I’ll do. And for sharing those thoughts so that people can have a sneak peek into the
internals of what I do mostly – I do a lot of things! :D – as a hobby on my spare time.

# What went wrong

Sooooooooo… What went wrong. Well, a lot of things! **spectra** was designed to build demo
productions in the first place, and it’s pretty good at it. But something that I have to enhance is
the *user interaction*. Here’s a list of what went wrong in a concrete way.

## Hot-reloading went wiiiiiiiiiiild²

With that version of **spectra**, I added the possibility to *hot-reload* almost everything I use as
a resource: shaders, textures, meshes, objects, cameras, animation splines, etc. I edit the file,
and as soon as I save it, it gets hot-reloaded in realtime, without having to interact with the
program (for curious ones, I use the straight-forward [notify crate](https://crates.io/crates/notify)
for registering callbacks to handle file system changes). This is very great and I save a **lot** of
time – Rust compilation is slow, and that’s a lesson I had learned from Céleri Rémoulade: keeping
closing the program, make a change, compiling, running is a wast of time.

So what’s the issue with that? Well, the main problem is the fact that in order to implement
hot-reloading, I wanted performance and something very simple. So I decided to use *shared mutable
smart states*. As a **Haskeller**, I kind of offended myself there – laughter! Yeah, in the
Haskell world, we try hard to avoid using shared states – `IORef` – because it’s not referentially
transparent and reasoning about it is difficult. However, I tend to strongly think that in some very
specific cases, you need such side-effects. I’m balanced but I think it’s the way to go.

Anyway, in Rust, shared mutable state is implemented via two types: `Rc/Arc` and `Cell/RefCell`.

The former is a runtime implementation of the *borrowing rules* and enables you to share a pointer.
The borrowing rules are not enforced at compile-time anymore but dynamically checked. It’s great
because in some time, you can’t know how long your values will be borrowed or live. It’s also
dangerous because you have to pay extra attention to how you borrow your data – since it’s checked
at runtime, you can crash your program if you’re not extra careful.

> `Rc` means *ref counted* and `Arc` means *atomic-ref counted*. The former is for values that stay
> on the same and single thread; the latter is for sharing the pointer between threads.

`Cell/RefCell` are very interesting types that provide *internal mutation*. By default, Rust gives
you *external mutation*. That is, if you have a value and its address, can mutate what you have at
that address. On the other hand, *internal mutation* is introduced by the `Cell` and `RefCell`
types. Those types enable you to mutate the content of an object stored at a given address without
having the exterior mutatation property. It’s a bit technical and related to Rust, but it’s often
used to mutate the content of a value via a function taking an immutable value.

> `Cell` only accepts values that can be copied bit-wise and `RefCell` works with references.

Now, if you combine both – `Rc<RefCell<_>>`, you end up with a shareable – `Rc<_>` – mutable –
`RefCell<_>` – value. If you have a value of type `Rc<RefCell<u32>>` for instance, that means you
can clone that value and store it everywhere in the same thread, and at any time, borrow it and 
inspect and/or mutate its content. All copies of the values will observe the change. It’s a bit like
C++’s `shared_ptr`, but it’s safer – thank you Rust!

So what went wrong with that? Well, the borrow part. Because Rust is about safety, you still need to
tell it how you want to borrow at runtime. This is done with the [`RefCell::borrow()`](https://doc.rust-lang.org/std/cell/struct.RefCell.html#method.borrow]
and [`RefCell::borrow_mut()`](https://doc.rust-lang.org/std/cell/struct.RefCell.html#method.borrow_mut)
functions. Those functions return a special object that borrows the pointed object as long as it
lives. Then, when it goes out of scope, it releases the borrow.

So any time you want to use an object that is hot-reloadable with my framework, you have to call
one of the borrow functions presented above. You end up with a lot of borrows, and you have to keep
in mind that you can litterally crash your program if you violate the borrowing rules. This is a
nasty issue. So far, I haven’t really spent time trying to fix that, but that something I have to
figure out.

## Resources declaration in code

This is a bit tricky. As a programmer, I’m used to write algorithms and smart architectures to
transform data and resolve problems. I’m given inputs and I provide the outputs – the solutions.
However, a demoscene production is special: you don’t have inputs. You create artsy audiovisual
outputs from nothing but time. So you don’t really write code to solve a problem. You write code
to create something that will be shown on screen or in a headphone. This aspect of demo coding has
an impact on the style and the way you code. Especially in crunchtime. I have to say, I was pretty
bad on that part with that demo. To me, code should only be about transformations – that’s why I
love Haskell so much. But my code is clearly not.

If you know the `let` keyword in Rust, well, imagine hundreds and hundreds of lines starting with
`let` in a single function. That’s most of my demo. In rush time, I had to declare a *lot* of things
so that I can use them and transform them. I’m not really happy with that, because those were data
only. Something like:

```rust
let outline_emblem = cache.get::<Model>("Outline-Logo-final.obj", ()).unwrap();
let hedra_01 = cache.get::<Model>("DSR_OTLINV_Hedra01_Hi.obj", ()).unwrap();
let hedra_02 = cache.get::<Model>("DSR_OTLINV_Hedra02_Hi.obj", ()).unwrap();
let hedra_04 = cache.get::<Model>("DSR_OTLINV_Hedra04_Hi.obj", ()).unwrap();
let hedra_04b = cache.get::<Model>("DSR_OTLINV_Hedra04b_Hi.obj", ()).unwrap();
let twist_01 = cache.get::<Object>("DSR_OTLINV_Twist01_Hi.json", ()).unwrap();
```

It’s not that bad. As you can see, **spectra** features a *resource cache* that provides several
candies – hot-reloading, resource dependency resolving and resource caching. However, having to
declare those resources directly in the code is a nasty boilerplate to me. If you want to add a new
object in the demo, you have to turn it off, add the Rust line, re-compile the whole thing, then run
it once again. It breaks the advantage of having hot-reloading and it pollutes the rest of the code,
making it harder to spot the actual transformations going on.

This is even worse with the way I handle texts. It’s all `&'static str` declared in a specific file
called `script.rs` with the same insane load of `let`. Then I rasterize them in a function and use
them in a very specific way regarding the time they appear. Not fun.

## Animation edition

Most of the varying things you can see in my demos are driven by animation curves – splines. The
bare concept is very interesting: an animation contains control points that you know have a specific
value at a given time. Values in between are interpolated using an interpolation mode that can
change at each control points if needed. So, I use splines to animate pretty much everything: camera
movements, objects rotations, color masking, flickering, fade in / fade out effects, etc.

Because I wanted to be able to edit the animation in a comfy way – lesson learned from Céleri
Rémoulade, splines can be edited in realtime because they’re hot-reloadable. They live in JSON files
so that you just have to edit the JSON objects in each file and as soon as you save it, the
animation changes. I have to say, this was very ace to do. I’m so happy having coded such a feature.

However, it’s JSON. It’s already a thing. Though, I hit a serious problem when editing the
orientations data. In **spectra**, an orientation is encoded with a
[unit quaternion](https://en.wikipedia.org/wiki/Quaternion#Unit_quaternion). This is a 4-floating
number – hypercomplex. Editing those numbers in a plain JSON file is… challenging! I think I really
need some kind of animation editor to edit the spline.
