# On traits privacy interfaces

I’ve been facing an issue several times in my Rust lifetime experience and never ever have
come up with a pragmatic solution to this problem yet. This problem comes up as follows:

  - Given a trait defined in a library, some code of the library uses its
    associated types, methods and/or constraints. **Those items use internal representations
    or dependencies that must not appear in the public interface of the library.**
  - That trait is important for the public interface because it gives a practical list of
    types that can be used with functions constrained with that trait.
  - Since we need that trait in the public interface, we **must make it public**.
  - However, **in Rust, public traits will expose their whole internals to the world.**

As you can see, we have a situation here. Rust doesn’t allow to expose a trait without
showing its internals. Imagine the following code:

```
pub fn a_cool_function<T>(id: usize, foo: T) where T: Foo {
  // …
}

pub trait Foo: Sized {
  type Cons: Sized;

  fn compute(self, a: Self::Cons) -> Self;
}

impl Foo for String {
  type Cons = char;

  fn compute(mut self, a: Self::Cons) -> Self {
    self.insert(0, a);
    self
  }
}

impl<T> Foo for VecDeque<T> {
  type Cons = T;

  fn compute(mut self, a: Self::Cons) -> Self {
    self.push_front(a);
    self
  }
}
```

We want to export `a_cool_function`. That function accepts as second argument *a type* that must
implement the `Foo` trait. In order for the function not to leak private symbols, `Foo` then must
be public. However, we don’t want to expose its internals (the `Cons` associated type nor the
`compute` method). Why we would want to hide those? I have several points:

  - If you look closely at the implementation, `a_cool_function` requires its second argument to
    implement `Foo`. Not exposing the internals would then force people to use stock implementors
    and will prevent them from providing new ones. This might be wanted for several reasons (unsafe
    traits, performances, etc.).
  - A more sensible reason: imagine that the associated type `Cons` required a bound on a very
    specific trait that is there only because of your implementation details / choices (like a trait
    from a dependency). That would leak that trait into the interface, which is not wanted.
  - As corollary, not exposing the internals of a trait would enable you to change the definition of
    the trait without inducing any breaking change, which is an interesting feature.

Currently, you can perfectly make a type public without exposing all its methods as public. Why not
having the same power with traits?

> There *is* a – non-ideal – solution to this problem: `#[doc(hidden)]` on each items to hide from
> the trait. Items tagged with that annotation won’t show up in the documentation, but they will be
> definitely usable if a crafty developer reads the source. Not a very good solution to me, thus.

# Pre-RFC: Enhanced trait privacy

It’s been a while since I’m looking for a good RFC to introduce this in Rust. This is some needed
jargon, so let’s explain a few terms first:

  - A *trait* is an open set of types that have common properties, stated by the *trait definition*.
  - A *trait definition* contains associated types, type variables, methods, associated methods
    and associated constants.
  - A *bound* is found in `where` clauses to constrain a type or a function.

Currently, when you implement `Display`, you implement the *trait*. The `fmt` function you implement
is part of its *trait definition*. When you write a function like
`fn show<T>(x: T) where T: Display`, here, `Display` is not a trait: it’s a *bound*.

*Bounds* are interesting, because you cannot directly manipulate them. They only appear when
you constrain a function or type. You can combine them, though, with the `+` operator:

```
fn borrow<'a, T>(x: &'a T) -> MyBorrow<'a, T> where T: Display + 'a
```

This example shows you that lifetimes can be used as bounds as well.

The idea of this RFC is to make a clear distinction between `Display` as trait and `Display` as
bound so that it’s possible to use a trait only in bounds position and not implementation. One
major avantage of doing so is to bring completely new semantics to Rust: exposing a trait as
public so that people can pick types that implement the trait without exposing what the trait is
about. This brings a new rule to the game: it’s possible to create ad hoc polymorphism that doesn’t
leak its definition.

The idea is that we love types and we love our type systems. You might come across a situation in
which you need to restrict the set of types that a function can use but in the same time, the
implementation of the trait used to restrict the types is either unsafe, or complex, or depends on
invariants of your crate. In my [spectra] crate, I have some traits that are currently public that
leak rendering dependencies, which is something I really dislike.

As a *prior art* section, here’s the wanted feature in Haskell:

```
{-# LANGUAGE FlexibleInstances #-} -- don’t mind this
{-# LANGUAGE TypeFamilies #-} -- this either

module Lol (
    Foo -- here, we state that we only export the typeclass, not its definition
  ) where

import Data.Text (Text, cons)

class Foo a where
  type Cons a :: *

  compute :: Cons a -> a -> a

instance Foo [a] where
  type Cons [a] = a

  compute = (:)

instance Foo Text where
  type Cons Text = Char

  compute = cons
```

Trying to use either the `Cons` associated type or `compute` function in a module importing `Lol`
will result in a compiler error, because those symbols won’t be accessible.

## What it would look like in Rust?

Currently, there is a *weird* privacy rule around traits. People not coming from Haskell might feel
okay about that, but I learned Rust years ago while being already fluent with Haskell and got
stunned at this (and I still have microseconds of *“Wait, do I not need a `pub` here? Oh yeah, nah
nah nah.”*) When you declare a trait as `pub trait …`, everything in its definition is automatically
`pub` as well.

This is so weird because *everything else* in Rust doesn’t work this way. For instance:

```
struct Bar; // here, Bar is not pub, so it’s private and scoped to the current module it’s defined in

pub(crate) struct Zoo; // not public either but can be used in other modules of the current crate

pub struct Point { // public
  pub x: f32, // public
  pub y: f32, // public
}

pub struct File { // public
  inode: usize // private
}

enum Either<L, R> { // private
  Left(L), // private
  Right(R), // private
}

pub enum Choice<L, R> { // public
  Left(L), // public (*)
  Right(R), // public (*)
}

// (*): enums require their variants to be public if they’re public for obvious pattern-matching
// exhaustiveness reasons

pub struct Foo; // public

impl Foo {
  fn quux(&self); // not public, only callable in the current module

  pub(crate) fn crab_core_is_funny(self) -> Self; // not public but callable from within this crate

  pub fn taylor_swift() -> Self; // public, callable from the crate and dependent crates
}
```

But:

```
trait PrivTrait { // private trait
  fn method_a(); // private
  fn method_b(); // ditto
  pub fn method_c(); // compilation error and wouldn’t make sense anyway
}

pub trait PubTrait { // public trait
  fn method_a(); // public, even without the pub privacy modifier!!!
  pub(crate) fn method_b(); // won’t compile
  pub fn method_c(); // won’t compile
}
```

To me, it would make much more sense for Rust to authorize this:

```
pub trait PubTrait { // public trait
  fn method_a(); // private, only usable from this module
  pub(crate) fn method_b(); // callable only from modules from this crate
  pub fn method_c(); // public
}
```

However, I know, I know. Turning this feature on would break pretty much **everyone’s** code. That’s
why I think – if people are interested by this feature – we should instead go for something like
this:

```
trait PrivTrait { // private trait
  fn method_a(); // private
  pub(crate) fn method_b(); // compilation error: the trait is private
  pub fn method_c(); // compilation error: the trait is private
}

pub trait PubTrait { // public trait
  fn method_a(); // public (backward compatibility)
  pub(crate) fn method_b(); // callable only from modules from this crate
  pub fn method_c(); // public, akin not to use the pub modifier
  priv fn method_d(); // private; only callable from this module
}
```

I’d like to point the reader to [this issue](https://github.com/rust-lang/rust/issues/8122). In its
foreword, [\@alexcrichton] rightfully explains that removing the `priv` keyword was great because it
has yielded a rule ever since – quoting him:

> *“I think that this would really simplify public/private because there's one and only one rule:
> private by default, public if you flag it.”*

I feel uncomfortable with the current trait situation because that rule has been broken. [This other
issue](https://github.com/rust-lang/rust/issues/8592) pinpoints the problem from another look:
traits shouldn’t set their items visibility based on their own visibilities. This yields weird and
unexpected code rules and precludes interesting design semantics – the one I just described
above.

I hope you liked that article and thoughts of mine. I might write a proper RFC if you peeps are
hyped about the feature – I have personally been wanting this for a while but never found the time
to write about it. Because I care – a lot – about that feature, even more than
[my previous RFC on features discoverability], please feel free to provide constructive criticism,
especially regarding breaking-changes issues.

Keep the vibes!

[spectra]: https://crates.io/crates/spectra
[\@alexcrichton]: https://github.com/alexcrichton
[my previous RFC on features discoverability]: https://phaazon.net/blog/rust-features-documentation
