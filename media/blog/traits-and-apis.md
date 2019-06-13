In this blog article, I want to explore a problem I’ve been facing from time to time in [luminance].

# The manual dispatch problem

The idea is simple: you are writing a crate and want to expose an API to people. You want them to
know which type they can use with a given operation (let’s call it `update`). However, the actual
implementation of this update function is not performed directly by your API but is deferred to a
*backend* implementation. Some people usually like to do that with several crates; in my case, I
really don’t care and let’s think in terms of types / modules instead.

There are several ways to do this. Let’s start with a nobrainer.

## The first attempt: dynamic dispatch

```rust
use std::marker::PhantomData;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Type {
  Int,
  UInt,
  Float,
  Bool
}

#[derive(Debug)]
pub struct Var<B, T> where B: ?Sized {
  name: String,
  _b: PhantomData<*const B>,
  _t: PhantomData<*const T>,
}

impl<B, T> Var<B, T> {
  pub fn new(name: &str) -> Self {
    Var {
      name: name.to_owned(),
      _b: PhantomData,
      _t: PhantomData,
    }
  }
}

impl<B, T> Var<B, T> where B: Backend, T: Interface {
  pub fn update(&mut self, value: &T) {
    B::update(self, value);
  }
}

trait Interface {
  const TY: Type;
}

impl Interface for i32 {
  const TY: Type = Type::Int;
}

impl Interface for u32 {
  const TY: Type = Type::UInt;
}

impl Interface for f32 {
  const TY: Type = Type::Float;
}

impl Interface for bool {
  const TY: Type = Type::Bool;
}

trait Backend {
  fn update<T>(var: &mut Var<Self, T>, value: &T) where T: Interface;
}

impl Backend for () {
  fn update<T>(var: &mut Var<Self, T>, _: &T) where T: Interface {
    match T::TY {
      Type::Int => println!("setting {} to int", var.name),
      Type::UInt => println!("setting {} to unsigned int", var.name),
      Type::Float => println!("setting {} to float", var.name),
      Type::Bool => println!("setting {} to bool", var.name),
    }
  }
}
```

Let’s dig in. The idea of this solution is to have a trait, `Interface`, that is used to create a
set of types that can be used in the API with the `update` function on the `Var` type. The
implementation is deferred to a backend via the `Backend` trait, that contains the interface of
the implementation. Basically, the `Var::update` function will select at compile-time which
backend to use, that will in its turn *observe* the type of the variable at runtime — see the
`match` block. This is not ideal as we will have branching. We would like a better way to do it.

## The second attempt: static dispatch

Instead of dynamically dispatching the types of the variable, we can play around with it at
compile-time. That requires some changes but in the end it’s pretty clear what we need to do:

```rust
use std::marker::PhantomData;

#[derive(Debug)]
pub struct Var<B, T> where B: ?Sized, T: ?Sized {
  name: String,
  _b: PhantomData<*const B>,
  _t: PhantomData<*const T>,
}

impl<B, T> Var<B, T> {
  pub fn new(name: &str) -> Self {
    Var {
      name: name.to_owned(),
      _b: PhantomData,
      _t: PhantomData,
    }
  }
}

impl<B, T> Var<B, T> where T: Interface, B: Backend {
  pub fn update(&mut self, value: &T) {
    T::update(self, value)
  }
}

trait Interface {
  fn update<B>(var: &mut Var<B, Self>, value: &Self) where B: Backend;
}

impl Interface for i32 {
  fn update<B>(var: &mut Var<B, Self>, value: &Self) where B: Backend {
    B::update_i32(var, value);
  }
}

impl Interface for u32 {
  fn update<B>(var: &mut Var<B, Self>, value: &Self) where B: Backend {
    B::update_u32(var, value);
  }
}

impl Interface for f32 {
  fn update<B>(var: &mut Var<B, Self>, value: &Self) where B: Backend {
    B::update_f32(var, value);
  }
}

impl Interface for bool {
  fn update<B>(var: &mut Var<B, Self>, value: &Self) where B: Backend {
    B::update_bool(var, value);
  }
}

trait Backend {
  fn update_i32(var: &mut Var<Self, i32>, value: &i32);
  fn update_u32(var: &mut Var<Self, u32>, value: &u32);
  fn update_f32(var: &mut Var<Self, f32>, value: &f32);
  fn update_bool(var: &mut Var<Self, bool>, value: &bool);
}

impl Backend for () {
  fn update_i32(var: &mut Var<Self, i32>, value: &i32) {
    println!("setting {} to int {}", var.name, value);
  }

  fn update_u32(var: &mut Var<Self, u32>, value: &u32) {
    println!("setting {} to unsigned int {}", var.name, value);
  }

  fn update_f32(var: &mut Var<Self, f32>, value: &f32) {
    println!("setting {} to float {}", var.name, value);
  }

  fn update_bool(var: &mut Var<Self, bool>, value: &bool) {
    println!("setting {} to bool {}", var.name, value);
  }
}
```

We change the definition of the `Backend` trait to have all the functions dispatched statically.
Then, using the `Interface` trait, we now have one information we didn’t have in the first example:
the actual, concrete type of the variable. We can then call the right function from the `Backend`
trait.

To sum up, because all of this is starting to be a bit confusing:

  - The `Interface` trait is the trait used to restrict the public API (i.e. which types can be
    used publicly).
  - The `Backend` trait is the trait to implement when providing the actual implementation.

However, if we add more types, that solution won’t scale easily. The problem is that we have the
typing information in several places (at the `impl` level and in a static list in a trait). It would
be much easier if we could, somehow, force an `impl` to exist in another trait. Basically, I want to
remove those `update_*` and use `impl`s instead.

## The third and final solution: inferred static dispatch

I have no idea how to call that way of doing but I like to think of it about inferred constraints.
The idea is almost the same as the second solution but instead of declaring the list of functions
that can be used in the implementation of the `Interface` trait, we will just create a generic
dependency between the `Interface` trait and `Backend`. The advantage will also be that we don’t
have to worry about the name of the function anymore, since it will be polymorphic.

Let’s go.

```rust
use std::marker::PhantomData;

#[derive(Debug)]
pub struct Var<B, T> where B: ?Sized, T: ?Sized {
  name: String,
  _b: PhantomData<*const B>,
  _t: PhantomData<*const T>,
}

impl<B, T> Var<B, T> {
  pub fn new(name: &str) -> Self {
    Var {
      name: name.to_owned(),
      _b: PhantomData,
      _t: PhantomData,
    }
  }
}

impl<B, T> Var<B, T> where T: Interface, B: Backend<T> {
  pub fn update(&mut self, value: &T) {
    B::update(self, value)
  }
}

trait Interface {}

impl Interface for i32 {}

impl Interface for u32 {}

impl Interface for f32 {}

impl Interface for bool {}

trait Backend<T> {
  fn update(var: &mut Var<Self, T>, value: &T);
}

impl Backend<i32> for () {
  fn update(var: &mut Var<Self, i32>, value: &i32) {
    println!("setting {} to int {}", var.name, value);
  }
}

impl Backend<u32> for () {
  fn update(var: &mut Var<Self, u32>, value: &u32) {
    println!("setting {} to unsigned int {}", var.name, value);
  }
}

impl Backend<f32> for () {
  fn update(var: &mut Var<Self, f32>, value: &f32) {
    println!("setting {} to float {}", var.name, value);
  }
}

impl Backend<bool> for () {
  fn update(var: &mut Var<Self, bool>, value: &bool) {
    println!("setting {} to bool {}", var.name, value);
  }
}
```

This solution is quite interesting because of the use of the type parameter in the `Backend` trait.
It enables you to implement the `Backend` trait and still observe (i.e. *know*) the type of the
variable you’re playing with. Compare with the first solution, where we were completely generic on
the `T` type.

In [luminance], I use the last solution to allow a clear distinction between a set of public types
and a set of matching implementations. Notice in the last solution the `Interface` trait, which is
now reduced to something pretty dumb. It would be easy for anyone to implement it for their own
types and then implement `Backend<TheirType> for TheirBackend`. Rust doesn’t offer a way to have
*sealed traits* so far, so my current solution to this is to mark the trait `unsafe` (to scare
people and tell them not to implement the trait). However, a clear and first-citizen language
construct for this would be highly appreciated.

[luminance]: https://crates.io/crates/luminance
