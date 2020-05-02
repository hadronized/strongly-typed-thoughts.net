Lately, I’ve been hacking on the next version of [luminance], luminance-0.40. It should be out
_“soon-ish”_ but in the meantime, I’ve been struggling a bit with some highly and strongly typed
code. I want to share something interesting I discovered with `rustc`. Especially, I haven’t seen
a mention of that property [in the book][0], so I’m happily sharing.

<!-- vim-markdown-toc GFM -->

* [What is a super trait?](#what-is-a-super-trait)
* [The weird stuff](#the-weird-stuff)
* [A small disgression to Haskell land](#a-small-disgression-to-haskell-land)

<!-- vim-markdown-toc -->

# What is a super trait?

The weird situation is about _super traits_. A super trait is a trait that must be implemented
for another trait to be usable, because it’s relied on. Traits can be thought of as
_constraints_, so a super trait is a bit like a _dependency_ when implementing a trait, and an
_implication_ when using a trait on which a super trait is declared. A trait can have zero to
several super traits (added with the `+` operator). For instance, imagine you have a trait `Alive`:

```rust
trait Alive {
  fn get_health(&self) -> Health;
}
```

Now imagine you want a trait to move someone or something around:

```rust
trait Move: Alive {
  fn go(&mut self, direction: Direction);
}
```

Here, you can see that:

- `Move` requires to implement `Alive`, because it’s a super trait. It’s a _dependency_.
- Because `Move` requires `Alive`, `Alive` is implied when you use `Move`. Indeed, that would be
  redundant to annotate a type `T: Move + Alive`, because an instance (implementor) for `Move`
  cannot exist without `Alive` to be implemented as well.

So now that we understand what super traits are, let’s get to the weird stuff.

# The weird stuff

When you implement a trait which has a super trait, do you think that:

1. Your implementation is valid _when the super trait is implemented_? After all, you could simply
  assume it is implemented. If it’s not, instances of your trait won’t be pickable.
2. Or your implementation _requires_ the super trait to be implemented?

The distinction is important. (1.) doesn’t require `rustc` to prove _when implementing a trait_
that the super trait is implemented. That will be required when using the trait. With (2.), `rustc`
will have to prove that the super trait is, first, implemented, before even considering the
implementation of the trait you’re making.

Rust currently uses (2.). If you `impl Move for Foo`, `impl Alive for Foo` must be in scope for
that implementor to be possible.

But… it would be interesting to actually have (1.). Imagine that you want to implement `Move` for
something a bit complex, like `Creature<T>`, but not all `Creature<T>` are alive. Only a subset
of them, and you can’t tell exactly when — i.e. you just cannot assume anything about `T`. So
what are you going to write?

```rust
impl<T> Move for Creature<T> {
  fn go(&mut self, direction: Direction) {
    // …
  }
}
```

This code will not compile, because you haven’t implemented `Alive for Creature<T>`. Remember that
the trait solver requires to prove super traits. However, and this is where all the interesting /
weird stuff happens:

```rust
impl<T> Move for Creature<T> where Self: Alive {
  fn go(&mut self, direction: Direction) {
    // …
  }
}
```

This compiles. It compiles because the `where` clause tells `rustc` that your implementor _will be
valid_ if used with `Creature<T>: Alive`. The distinction is really subtle, but is, to my opinion,
very powerful. With the `where` clause, you state that `Move` is implemented for any `Creature<T>`
that is also `Alive`, but you don’t require them all to be `Alive`! You could implement `Alive`
for a bunch of creatures, like `Creature<Vampire>` and `Creature<CloudDog>`.

So, I remember having read somewhere (maybe in some Rust book, but I’m not quite sure) that the
`where Self: _` clause was not really useful, but in our case, you can see that it allows to
express a completely different semantics.

> You could also have used `Creature<T>: Alive` in place of `Self: Alive`, as here,
> `Self = Creature<T>`.

# A small disgression to Haskell land

In Haskell, that code requires to use `UndecidableInstances`. I don’t know _exactly_ why, but GHC
states that the constraint (`Alive (Creature a)`) is no smaller than the instance head
(`Move (Creature a)`), and this is not permitted, as being undecidable. Enabling the
`UndecidableInstances` GHC extension will make it possible to compile:

```haskell
class Alive a where
  getHealth :: a -> Health

-- The super class is declared on the left side of the => (the parenthesis are
-- optional, but I’m used to put them all the time, as they are required when you
-- have more constraints).
class (Alive a) => Move a where
  go :: Direction -> a -> a

-- This instance requires UndecidableInstances to compile.
instance (Alive (Creature a)) => Move (Creature a) where
  go = -- …

instance Alive (Creature Vampire) where
  getHealth = -- …

instance Alive (Creature CloudDog) where
  getHealth = -- …
```

I’m not quite sure why this _very exact_ situation requires `UndecidableInstances` though. In this
case, it seems fine.

I hope you’ll have learned something with this small yet fun type theory candy. Keep the vibes!

[luminance]: https://github.com/phaazon/luminance-rs
[0]: https://doc.rust-lang.org/book/ch19-03-advanced-traits.html#using-supertraits-to-require-one-traits-functionality-within-another-trait
