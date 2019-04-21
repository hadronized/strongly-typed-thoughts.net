# splines-1.0.0: the first Release Candidate

Oh hello there! It’s been a while since I wrote on here. My last article was posted on February
25th and then I decided it was time to blog more about what I do. Especially, I think I will try to
blog more often, even if it’s about trivial things, as long as I share my thoughts.

So today, I’m going to talk about the [splines] crate. And more specifically, the [splines-1.0.0-rc.1]
release candidate I uploaded today on crates.io.

## Foreword: you said splines?

Maybe you’re wondering what a spline is, in the first place. A spline is a mathematic curve that is
defined by several polynomials. You can picture them mentally by several small and simple curves
combined to each others, giving the curve an interesting shape and properties. Now why we want
splines is easy to understand: imagine a curve, something smooth and a bit complex (maybe even with
loops). Now, imagine you want to make an object move along that curve. How do you represent that
curve and how to you “make something advance along it?”

Splines are defined by several “control points”, also known “keys”, “knots” or whatever makes sense
to you. Those are just points of interest that will have a direct impact on how the curve will bend
and behave. Most of control points have the interesting property that the curve **will** pass
through them, allowing you to define points you want your object to pass by, for instance. Not all
control points have that property, though.

Now all the magic happens with those control points: when you want to sample a value at a given
control point, you just get back the value carried by this control point… but when you want to
sample in between two control points, for instance, you get back an *interpolated value*. That
interpolation is the key to why we want splines. Several interpolation mechanisms exist and I’m sure
you know at least two:

  - **Constant interpolation**: it means that given two control points, sampling at the first
    control point or any part between the control points will give you the carried value of the
    first control point. You will get the carried value of the second control point if you sample at
    its sampling value. That might be like a useless feature but it is very handy when designing and
    implementing “stop-by” movement with camera.
  - **Linear interpolation**: this one you might know it, especially if you attended math courses at
    high-school. Linear interpolation is often referred to as “drawing a straight line”. And
    typically, “everything linear” can often graphed with a straight line! Thank about linear
    complexity (*O(n)*) and what it looks like on a graph. Linear interpolation that gives you a
    strict blending of arguments depending at which distance you sample from them. If sample at
    `t = 0`, you get 100% of the first value and 0% of the second value. If you sample at `t = 0,5`,
    you get an equal amount of both value (so you get the mean between both value). At `t = 1`, you
    get 0% of the first value and 100% of the second value. Simple but really useful.
  - **Cosine interpolation**: this one is known by people who are used to animation, graphics,
    motion design, etc. Cosine interpolation gives you a smoother interpolation than the linear one
    and provides a first derivative that is more interesting (i.e. it translates to *acceleration*).
  - Others: [splines] provide you with some other interpolation modes and some (that I really want!)
    are missing. For instance, cubic [Bézier interpolation] is something I want and that I already
    have coded in the Haskell version of the [splines] crate. It should be easy to add but so far,
    no one has asked for them and I don’t need them, so… ;)

> Keep in mind that a spline is a just an abstractio mathematical object and that you can use it for
> lots of concepts and situations. For instance, in demoscene productions of mine, I use splines for
> camera movement, animation transitions, color-blending and more!

## The splines crate

The [splines] crate has been around for some time now and I received some feedback from people on
the [GitHub page](https://github.com/phaazon/splines) asking for more features. Basically, the crate
can now:

  - Support failible sampling more sanely (instead of panicking on some very trick situations, you
    get a `None`).
  - Support for polymorphic sampling types (you have `f32` but also `f64` or whatever).

I think something interesting to dissect in this blog article is the last feature about polymorphic
sampling types and what I had to do in order to make it happen. Also, this is highly correlated to
a design choice I have made months ago: supporting foreign crates to interpolate specific types is
done inside the [splines] crate, not in other crates. I will discuss why just bellow.

## Polymorphic sampling

Basically, in [splines], prior to `1.0.0`, a spline was polymorphic type mapping a key of type `f32`
to a control point of type `Key<V>`, `V` being the carried value. You then manipulate a `Spline<V>`.
The problem with that is whenever someone wants to use a different time, like, `f64` or something of
their own.

My first solution was to turn the spline type to `Spline<T, V>`. That would allow people to use both
`f32` and `f64` as sampling type. The problem kicks in with the code handling those sampling values.
For instance, for the `Interpolation::Cosine` interpolation mode, I need to take the cosine of a
value which type is either `f32` or `f64`. How can we do that in a generic and polymorphic maneer?

> [num-traits] to the rescue!

Yeah, so was my first thoughts. I then re-wrote most of the interpolating code using [num-traits]…
and then decided to tackle all the feature gates. Because, I haven’t told you yet, but [splines] has
several feature gates allowing you to use, e.g., implement the `Interpolate` trait for some famous
crates’ types ([nalgebra], [cgmath]), enable [serde] serialization, etc. One feature gate that is
important to me as a demoscener is the `"std"` feature gate, that, if not defined, makes the
[splines] crate compile with the `no_std` feature.

And here comes the first problems. The [num-traits] has a trait that isn’t compatible with `no_std`,
the [`Float`](https://docs.rs/num-traits/0.2.6/num_traits/float/trait.Float.html) trait. I then sat
in front of my computer and [thought](https://phaazon.net/media/uploads/trying_to_think.jpg). I came
to the realization that whatever library I would use for that trait, I would always get my hands
tied by the *contracts of the public interface of that crate*, for a feature that is almost central
to the whole [splines] crate. It quickly became obvious that I couln’t use [num-traits].

> Yes, I know about the [`FloatCore`](https://docs.rs/num-traits/0.2.6/num_traits/float/trait.FloatCore.html)
> trait. However, I was stuck with `FloatConst` right after that and I just wanted to clean the
> interface.

The monomorphic version of [splines] is easy to implement for `no_std`, but now I’m stuck because of
some simple traits? Naaah.

So I decided to write my own traits. Enter: `Additive`, a simple trait with no content but `Add` and
`Sub` as supertraits (and some non-important ones for comprehension here), `One`, a trait that
provides the neutral element of the multiplication for a given type (i.e. the multiplication monoid)
and the `Linear<T>` trait, providing linear combinations, mandatory for all kind of interpolations
mentioned earlier.

Those traits have universal implementors so that you don’t have to implement them. And enabling a
feature gate for, e.g., [cgmath] will also implement those traits accordingly in an enough
polymorphic way that you shouldn’t have to ever worry about them.

Just a little remark of mine: implementing the support for [cgmath] was pretty simple and
straight-forward. As feared (because I used this crate), [nalgebra] was way harder to get right —
and I even decided not to implement the `Point<..>` type because I’ve been struggling all the
afternoon with error types that reminded me old and dark times with C++ Boost library. I’ve already
discussed that with people from the [nalgebra] world and none seemed to agree with me that this
crate is *a little bit too overengineered*. For instance, adding the support for [nalgebra] in
splines also adds several mandatory dependencies — i.e. [num-traits] and [alga]. It’s not that bad,
but if you want to play with simple vector spaces, I really do not recommend [nalgebra] as it’s a
really convoluted library with lots of `type` aliases and types with infinite counts of type
variables. Maybe it’s just a confirmation bias of mine, because I’ve always used (and written, back
then in C++) linear libraries that were both simple and fast. [nalgebra] makes me [think of this and
this is driving me crazy](https://www.boost.org/doc/libs/1_55_0/libs/geometry/doc/html/geometry/design.html).
I’m not saying it’s bad. I’m saying it’s not for me and that I wouldn’t recommend it for people
wanting to do simple things — and yes, video game still falls in that *simple things* bag.

## Let’s talk about feature gates

So, the design of feature gating in [splines]. I know it might feel a bit weird to have gates like
`"impl-nalgebra"` or `"impl-cgmath"` but to me it makes lots of sense. For a simple reason: a crate
exposes several types and functions via its public API. Everything that is not public is not for a
very good reason. Mostly, *invariants*. An invariant is something that must remain true _before_ a
function / morphism / whatever is called and _after_. What happens in between can actually break
that invariant. The idea is that *“If a function breaks internally an invariant, it must restore it
before returning”*, so that the API remains safe to use.

Invariants are central in my way of thinking. Everytime I write some code, everytime I design an
interaction between several piece of algorithms, crates or even cross-language and cross-systems, I
always asks myself *“Am I breaking an invariant here? Is it possible to fuck up a state or a hidden
property somewhere?”*

In Rust, invariants are not a first-class citizen of the language (have a look at how the [D]
language handles `invariant`: it’s interesting!). However, they still exist. Imagine this piece of
code:

```rust
pub struct Even(pub u64);

impl Even {
  pub fn new(nth: u64) -> Self {
    Even(nth * 2)
  }
}

impl Add<u64, Output = Even> for Even {
  fn add(self, rhs: u64) -> Output {
    Even(self.0 + rhs * 2)
  }
}
```

Imagine this is wrapped in a crate `num-even`. Adding a `u64` to an `Even` gives you the nth next
even number. All of this is cool. But there is an invariant. The carried `u64` by `Even` must…
remain even. If at some time a user handle a `Even` with an odd `u64` inside of it, something has
gone wrong terribly. And with this current implementation, it’s very easy to break such an
invariant. Consider:

```rust
use num_even::Even;

fn main() {
  let mut a = Even::new(1); // the 1st even number
  a.0 += 1; // oooooooh, fuuuuuuu-
}
```

I had long heated debates with people on IRC about why this kind of library should be patched. The
main argument of people who would think it shouldn’t be patched is “Yeah but we want users to have
access to the underlying objects in any way they want.” I think invariants matter most. The patch
version is actually a negative diff:

```rust
pub struct Even(u64);
```

Done. Here, the invariant cannot be broken anymore because people cannot create or modify `Even`
by hand. Constraining is powerful. You should try it. :)

[splines] holds keys in a `Spline<..>` that must remain sorted. Hence, you don’t have a direct
access to the keys nor you can mutate them. Mutating keys would imply resorting the keys in a
smart way, which is something I haven’t needed yet.

So, that’s all for me for today. If you have any question about [splines], please feel free to open
an issue on GitHub. Also, if you’re interested in trying it, please have a look at the
[splines-1.0.0-rc.1] release candidate. It contains everything you need to get started!

> The documentation is not up to date and some is missing, but it should be enough to start.

As always, keep the vibes!

[splines]: https://crates.io/crates/splines
[splines-1.0.0-rc.1]: https://crates.io/crates/splines/1.0.0-rc.1
[Bézier interpolation]: https://en.wikipedia.org/wiki/B%C3%A9zier_curve
[num-traits]: https://crates.io/crates/num-traits
[nalgebra]: https://crates.io/crates/nalgebra
[cgmath]: https://crates.io/crates/cgmath
[serde]: https://crates.io/crates/serde
[alga]: https://crates.io/crates/alga
[D]: https://dlang.org/spec/contracts.html#Invariants
