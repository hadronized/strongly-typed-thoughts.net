Rust has this cool feature called *`impl` block*. An `impl` block is just a scope that introduces a
way to augment a type with methods – do not confuse `impl` blocks with *trait* `impl`s, used to
implement a given trait.

The syntax is the following:

```rust
pub struct MyId(u32);

impl MyId {
  /// A method that creates a null identifier.
  pub fn nil() -> Self {
    MyType(0)
  }

  /// A method that checks whether an ID is odd (why not?).
  pub fn is_odd(&self) -> bool {
    self.0 % 2 != 0
  }

  // some other methods…
}
```

This syntax gives you the *dot notation* and the *static method* call syntax:

```rust
let x = MyId::nil(); // static method call

println!("is it odd? {}", x.is_odd()); // dot notation
```

Most of the time, you’ll want `impl` blocks for:

  - The *dot notation*.
  - Sharing type variables.

# Type variables in impl blocks

A *type variable* is a variable that holds a type. People coming from languages like C++, Java,
C#, Python, D, etc. are used to name those *template parameters* or *type parameters*.

```rust
pub struct List<T> {
  // hidden
}

impl<T> List<T> {
  // hidden
}
```

`T` here is a type variable, and you can see that you can have an `impl` block over `List<T>`. What
that means is that any declared method will have a hidden type `Self = List<T>`. If you declare a
static method, `Self` will be set as `List<T>` and if you declare a regular method, `self`, `&self`
or `&mut self` will have types – respectively – `List<T>`, `&List<T>` and `&mut List<T>`.

That sharing is even more pronounced when you start to deal with more complex data types and want to
restrain their type variables with *trait bounds*. For instance, imagine that you want an ordered
list:

```rust
pub struct OrderedList<T> {
  // hidden
}
```

You could implement `OrderedList<T>` for any `T`, but it’s very likely that for *a lot* of methods,
you’ll need `T` to be comparable. So you can do this, for example:

```rust
impl<T> OrderedList<T> where T: PartialOrd {
  // hidden
}
```

All the methods and static methods will have `Self` an alias to `List<T> where T: PartialOrd`. This
will drastically help you when writing methods that all require the trait bound.

Note that you can have several `impl` blocks for any kind of refactoring. For instance, if you
*also* want to have two methods that don’t need the trait bound, you can go and add another `impl`
block that looks like:

```rust
impl<T> OrderedList<T> {
  pub fn new() -> Self {
    // hidden
  }

  pub fn singleton(sole: T) -> Self {
    // hidden
  }
}
```

Now let’s try something a bit more complex. Let’s try this:

```rust
trait Foo<E> {
  fn foo(&self, x: E);
}

impl<T, E> OrderedList<T> where T: Foo<E> {
  fn invoke_foo(&self, e: E) {
    for t in self {
      t.foo(e);
    }
  }
}
```

This won’t work and rustc will complain that `E` is not contrained enough (it doesn’t appear in
`Self`, it’s only used in a trait bound on `T`). So what’s the problem?

> You can test [here](https://play.rust-lang.org/?gist=993db37f3544929db973861075650158&version=stable&mode=debug&edition=2015)

# Enter surjection and injection

The problem with that last block is that `E` is not constrained on `OrderedList<T>` and is. If you
don’t know what it means, don’t freak out: I’m going to introduce every concepts you need to know.
Hang on, it’s gonna be a bit math–ish, but that is worth it.

## Surjection

In math, a *surjection* is a property about set morphisms. For a pair of sets `C` and `D` and a
morphism `f : C -> D`, we say that this morphism is *surjective* if all elements from `D` appear in
a `f` application. This is weirdly said, so let’s say it another way: if for all the elements 
`y` in `D`, you can find *at least one* element `x` in `C` so that `f(x) = y`. Another way to
swallow that down if you’re still choking is by drawing the `C` and `D` sets as big bags with a few
elements in there. The morphism `f` applications are just lines from objects from `C` to `D`. If all
elements from `D` have at least one arriving arrow, the morphism is surjective.

We write surjection this way:

    Ɐy ∈ D, ∃x ∈ C, f : C -> D / f(x) = y

And we read it this way:

  - `Ɐy` means *for all* `y`.
  - `∈ D` means *is an element of* `D`. So `Ɐy ∈ D` reads *for all y that is an element of D*.
  - `,` often reads as juxtaposition.
  - `∃x` means *there exists* `x`.
  - `f : C -> D` is just the type definition of the `f` morphism.
  - `/` is often used instead of `,` to delimit the equation and the hypothesis; read it as
    `so that`.

So this whole math stuff reads:

> For all `y` in `D`, there exists `x` in `C` so that `f(x) = y`.

It’s quite a non-intuitive notation when you don’t know about it but you can see the concepts are
pretty simple to understand.

You might ask, what is the point of knowing that in our case? Here, `Foo` is a type-level function.
Rust has decided to go with the _mainstream_ way to note that (which is another topic and should be
the topic of another blog post, to be honest), so yeah, it’s a bit uglier than in Haskell, but it’s
a type-level function. You can picture it as `Foo : E -> Foo<E>`. You can easily see that if you
are writing the body of a function and that you have a bound like `T: Foo<E>` available, you know
that you have type `T` that implements `Foo<E>`, so there’s definitely one type to substitute `E`
with (otherwise your function cannot be called), but there could be several ones. This means that
traits are surjective.

Another way to see why it’s surjective: if I give you `Foo<u32>`, you know that there’s at least one
type implementing it, but you don’t know which one, since it’s ambiguous.

```rust
struct A;
struct B;

impl Foo<u32> for A {
  // hidden
}

impl Foo<u32> for B {
  // hidden
}
```

If I give you `Foo<u32>` and tell you *“It’s in a bound position of a function”*, you know that
if the function gets called, there’s at least one type implementing `Foo<u32>`… But which one is it?
`A`? `B`?

## Injection

Injection is the *reversed* version of surjection. It tells you that all the elements from
`C` have a departing arrow (i.e. morphism application) ending in `D` **and** the elements in `D`
have zero or one arriving arrow. Another way to say it is that for all pairs of `x` and `y`
in `C`, `f(a) = f(b)` implies `a = b`, which means that you cannot have two inputs mapping to the
same result since if two applications of the morphism yield the same result, it means that the
inputs have to be the same.

    Ɐ(x, y) ∈ C, f : C -> D / f(x) = f(y) => x = y

  - `=>` is the math notation to state *implication*.


Two interesting properties of injections:

  - Some elements from `D` (which is called the *codomain* of the morphism while `C` is its
    *domain*) might not have any arriving arrow.
  - If you have an arriving arrow in the codomain, then you can easily reverse its direction and
    move backwards to the element in the domain because it’s not possible that two elements map to
    the same element.

The contrapositive is also interesting:

    Ɐ(x, y) ∈ C, f : C -> D /  x ≠ y => f(x) ≠ f(y)

It’s easy to imagine an injection in Rust. For instance, the function transforming a boolean into
an integer is an injection. Every value in the domain (`bool`) have an arrow into the codomain
(`u8` for instance):

  - `f(false) = 0`.
  - `f(true) = 1`.

You can also see `0` has a single arriving arrow, and if you take it backwards, you end up on
`false`. Same thing happens for `1` and `true`. Finally, you can see that `3`, `64` or `42`, which
are definitely in the codomain, have no arriving arrow. We are dealing with an injection.

What’s interesting with our specific typesystem problem is that if we had injective traits, that
would mean that given `Foo<T>`, there’s only one type that can implement this trait. You would then
be able to take the bound backwards and recover the type, removing the ambiguity.

> This is not correctly possible in Rust. If you’re interested, Haskell can do it via
> [injective type families](https://ghc.haskell.org/trac/ghc/wiki/InjectiveTypeFamilies) or
> functional dependencies, for instance.

### A small digression on bijection

For curiosity only, bijection is just the superposition of both the properties. If you have a
morphism that is both surjective and injective, it is bijective, and any element in the domain
gives you one element in the codomain and any element in the codomain gives you one element in the
domain an element in the codomain must have exactly one arriving arrow.

# Back to our impl block problem

```rust
trait Foo<E> {
  fn foo(&self, x: E);
}

impl<T, E> OrderedList<T> where T: Foo<E> {
  fn invoke_foo(&self, e: E) {
    for t in self {
      t.foo(e);
    }
  }
}
```

This is our initial problem. As you can see here, the `impl` block has two variables: `T` and `E`.
`T` here means that we’re implementing methods *for all* the `T`s – i.e. `Ɐ`. However, we’re not
implementing it for all the `E`. We would like to state that *we need a `E` to exist*, which means
that *there exists* an `E` – i.e. `∃`. The current syntax doesn’t support this.

However, what happens if we do this:

```rust
impl<T> OrderedList<T> {
  fn invoke_foo<E>(&self, e: E) where T: Foo<E> {
    for t in self {
      t.foo(e);
    }
  }
}
```

Here, you can see that since we can provide `E` by the caller of the `invoke_foo` function, we don’t
require an injective bound: we narrow all the possible types to one provided by the caller of the
function.

> You can [test it by yourself here](https://play.rust-lang.org/?gist=d3c84c52f211736cff956c22ba72a741&version=stable&mode=debug&edition=2015).
> The `Clone` trait was added so that we can actually loop with the input argument.

# Intuition

All of this made me think about why do we even use `impl` blocks. I mean, Haskell has been around
for roughly 30 years and we never seen the need for it arise. Rust has a very special way to express
typeclasses (Rust’s traits have an implicit type parameter, `Self`, which is a sort of limitation
that might get fixed in the future). If we forget about the *dot notation*, I truly think that I
would drop `impl` blocks for several reasons:

  - No solution currently exists to the problem this blog entry discusses with `impl` blocks so far.
  - Documentation might become very hard to read because it makes every methods dependent on an
    `impl` block in the documentation. This refactoring might be interesting for code readers and
    writers but is, to me, truly a nightmare to people reading the documentation – it kind of makes
    reading the documentation *stateful*, which is ridiculous. However, this point might be a very
    interesting `rustdoc` feature request! ;)
  - On a general note, I’m not really adding any constraint to the `impl` blocks because those
    depend mostly on the functions / methods and I find it utterly stupid to create `n` blocks for
    `n` functions. So I basically just spawn the most general form of `impl` block and put the 
    constraints next to the methods.
  - I often confuse them with `impl` trait implementors when reading the code from someone else.

What would be really great would be to allow people to create function with a `self` argument
*without `impl` block*. You could still have them around for compatibility purposes and for people
who actually enjoy them, but to me those are a bit useless and boilerplate.

> Again, I’m only talking about `impl` blocks. `impl` for trait are actually quite nice since you
> can see them and spot them easily (as the `instance` keyword in Haskell).

That’s all for today. Thank you for having read me. Keep the vibes, and write RFCs!
