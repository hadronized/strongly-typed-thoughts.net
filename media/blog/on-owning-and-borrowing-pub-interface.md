> Disclaimer: this blog is about [Rust] and some of its intrinsics semantics, along with software
> design and architecture – especially for public interfaces like APIs. However, you may find
> it interesting to apply the concepts to any language that would support those ideas directly or
> indirectly.

I’ve been writing on a few examples code lately to add to documentations of some crates of mine. I
write a lot of code that creates new objects that need other objects in order to be built. Most of
the APIs you can see around tend to love the borrow principle – and I do. The idea is simple:

  - If you want to create a new type that, for instance, must own a `String`, most APIs tend to
    agree that the constructor of your type should take a `&str` or some sort of borrowing – typical
    way to do that is to use the `AsRef` trait, that gives you more power (more about that below).
    You can then just clone the `&str` in your function.
  - Apply that principle to pretty much any kind of object you need but not types that don’t have
    easy borrowing semantics. For instance, `Rc<_>` already carries sharing and dynamic borrowing
    semantics, so I wouldn’t be surprised to find a constructor that takes a `Rc<_>` instead of a
    `&Rc<_>`.

Depending on the crate you look at, the authors and how they see things, you might find a lot of
ways to pass that string to your constructor. Let’s get technical. Especially, I want this blog post
to give people a hint at how they should shape their APIs by driving the types with semantics.

# The base code

```
struct Person {
  ident: String
}
```

Very simple to get started. We have a `Person` type that carries the name of that person. We don’t
want to expose the internals of the type because we might add a lot of other things in there and
allowing people to pattern-match a `Person` would break their code when we add more things.

```
impl Person {
  pub fn new(name: String) -> Person {
    Person { name }
  }
}
```

That works. This constructor works by *move semantics*: it expects you to move a `String` in in
order to copy it into the `Person` value. Move semantics are clear here: the *client* must allocate
a `String` or move one already allocated by someone else. This can get a bit boring. Imagine:

```
let someone = Person::new("Patrick Bateman"); // this won’t compile
```

`Person::new("Patrick Bateman")` doesn’t typecheck because `"Patrick Bateman"` has type `&'static
str` here. It’s not a `String`.

# Drive the allocation from within your code

So how can we fix the code above to have it compile?

```
let someone = Person::new("Patrick Bateman".to_owned()); // okay now
```

`"Patrick Bateman".to_owned()` makes a use of
[`ToOwned::to_owned`](https://doc.rust-lang.org/std/borrow/trait.ToOwned.html#tymethod.to_owned) to
clone the string. You could also have used
[`Into::into`](https://doc.rust-lang.org/std/convert/trait.Into.html). However, we’re still
requiring clients of our API to perform the allocation – or move in, which is not ideal. A typical
solution to this is to use a borrow and clone when needing an owned version.

```
impl Person {
  pub fn new(name: &str) -> Person {
    Person {
      name: name.to_owned()
    }
  }
}
```

This API enables the current code to compile:

```
let someone = Person::new("Patrick Bateman"); // okay because 'static is a subtype of 'a
```

We’re all good now, right? Not exactly: what happens if we try to pass the initial `String` we had
when we moved something in?

```
let x: String = …;
let someone = Person::new(x); // not okay now since it’s a type mismatch
```

To fix this, we need to pass a `&str` out of a `String`. Since
[`String` implements `Deref<Target = str>`](https://doc.rust-lang.org/std/ops/trait.Deref.html#impl-Deref-3),
it’s quite straight-forward:

```
let x: String = …;
let someone = Person::new(&x); // okay, here &x derefs to &str
```

> You can also use the more explicit `String::as_str` method.

A nice trick here to be able to pass both `&str` and `String` is to use the [`AsRef`] trait.

```
impl Person {
  pub fn new<N>(name: N) -> Person where N: AsRef<str> {
    Person {
      name: name.as_ref().to_owned()
    }
  }
}
```

In this case, `AsRef<str>` is implemented for both `&str` (it just returns itself) and `String` – it
just deref / uses the `as_str` method).

However, you can still see a problem here. If we keep using `x` after the call to `Person::new`,
this code is actually okay and we can move on. If we don’t need `x` afterwards, we’re just wasting
an opportunity to move in instead of allocating!

# Drive the allocation from your code and allow for moving in

Clearly, to me, the perfect API would enable you to pass borrows and ask the API code to clone for
you or just accept the memory region you provide (i.e. you move something in). The idea is then to
accept either `&str` or `String` in our case, and clone only a `&str`… There are several traits and
types that provide that feature.

## `ToOwned` + `Cow`

[`Cow`] – for Clone on write – is a very interesting concept. It encodes that you’re either
borrowing some data or that you’ve already borrowed it. See it as:

```
enum Cow<'a, T> where T: 'a + ?Sized + ToOwned {
  Borrowed(&'a T),
  Owned(T::ToOwned)
}

impl<'a, T> Cow<'a, T> where T: 'a + ?Sized + ToOwned {
  pub fn into_owned(self) {
    match self {
      Cow::Borrowed(b) => b.to_owned(),
      Owned(o) => o
    }
  }
}
```

Now, the interesting part: it’s possible to go from `&'a str` to `Cow<'a, str>` and from `String`
to `Cow<'a, str>` by using `Into` implementors. That enables us to write this:

```
impl Person {
  pub fn new<'a, N>(name: N) -> Person where N: Into<Cow<'a, str>> {
    Person { name: name.into().into_owned() }
  }
}
```

This code will move in a `String` if you passed one and clone if you passed `&str`. The following
lines of code compile and work as a charm:

```
let _ = Person::new("Patrick Bateman");

let dawg = "Dawg";
let _ = Person::new(format!("Doggo {}", dawg));
```

What’s interesting here is that all cases are covered:

  - The client **has the choice** to either allocate – or move in – or borrow.
  - It’s even possible to implement `Into<Cow<str>>` on other types to pass other kind of arguments.

There’s just a little nit: `Cow::into_owned` obviously patterns match on the variant, inducing a
small yet present runtime overhead. We tend to prefer using `Cow<_>` to dynamically dispatch the
decision to clone at runtime, while in our case, it’s more about a static choice (which API function
version to use).

## `Into`, as simple as it gets

If you look at the `Into<String>` implementors, you’ll find `impl Into<String> for String` –
actually, this is a
[blanket implementor](https://doc.rust-lang.org/std/convert/trait.From.html#impl-From%3CT%3E-10) –
and `impl<'a> Into<String> for &'a str`. That implements the same semantics as `Cow<str>`, but at
the type level, removing any remaining runtime overhead.

The new code is even simpler:

```
impl Person {
  pub fn new<N>(name: N) -> Person where N: Into<String> {
    Person { name: name.into() }
  }
}
```

Obviously, there are drawbacks:

  - You cannot express any lifetimes with this `Into<String>` trait bound. If you need, for any
    reason, to dynamically check whether to clone or not, the `Cow<str>` is the right decision. If
    you know you’ll always need to allocate, go for `Into<String>`.
  - `Into<String>` is vague and some types might not even implement it.
  - `Into<String>` doesn’t allow for cheap reading while `Cow<str>` does. `Into<String>` requires
    you to move or allocate prior to reading.

# Let’s draw a conclusion

What I wantedd to highlight in this blog post is that `&_`, `AsRef<_>`, `Cow<_>`, and `Into<_>` all
have different semantics that can be used to *encode* different contracts in public interfaces.

  - `&T` means that you don’t require your client to clone nor move because you *might* only perform
    read-only computations on `T`. That gives some intuitions to your clients:
      - You don’t require them to own `T`.
      - If any allocation must be performed, the client doesn’t have to worry about it.
      - You force them to use a reference. It can have interesting consequences. Imagine
        `&[Something]`. This carries one information more: contiguity of the input data – slices are
        contiguous.
  - `Q: AsRef<T>` is a `&T` on steroids in terms of software design. It gives more power to the
    client as they’ll be able to pass more types in your functions. However, you now introduce a
    polymorphic API. The semantics are the same: you plan to perform only read-only computations or
    accept not to use a client-provided move-in value if you need to own something. Keep in mind an
    important *hidden property* here: because you accept values to be moved in while you just have
    a read-only contract on them, you also accept clients values to be *dropped* by corollary.
  - `Cow<T>` states that the client can provide a borrow or an owned variable and you will take
    advantage of that **at runtime**. This is important as it gives a good idea about how the memory
    footprint of the function works: it will take decision at runtime whether or not it needs to own
    memory.
  - `Q: Into<T>` has vague and large semantics but can be used to encode the fact that you *want*
    owned data, but you accept your clients to provide a borrow as long as you can clone. If they
    provide you with an owned data, you’ll just do nothing (move in).
  - (bonus) `Q: IntoIterator<Item = T>` is also something you can use for the slices thing just
    above. If (and only if) you don’t care about contiguity or will just inspect values one by one,
    this is the interface to go. This interface gives hints that your function might iterate through
    the input and perform (perhaps heavy) computation on each items before stepping to the next one.
    Using `IntoIterator` instead of `Iterator` enables you the same kind of trick you have with
    `AsRef<_>` vs. `&_`: you can take a `Vec<_>` directly instead of the iterator directly. Keep in
    mind that this might not have sense for some types, especially if they have several iterator
    interfaces. The `str` type has for instance the
    [str::bytes](https://doc.rust-lang.org/std/primitive.str.html#method.bytes) method that gives
    you an iterator over bytes and the
    [str::chars](https://doc.rust-lang.org/std/primitive.str.html#method.chars) method that yields
    an iterator over UTF-8 characters.

This list gives you a good idea about what interface you should use in your public interfaces.
Read-only? Owned data? Read and maybe write? All those semantics are visible through those types and
traits, you should definitely try to wrap your finger around using all of them. Of course, if you’re
just writing a small utility function that needs to borrow the `.name` of a `Person`, passing in a
`&Person` seems completely sound. Most of the time, if you don’t know where the data comes from,
be the more inclusive and generic as possible.

I hope you like that small article, and as always, keep the vibes.

[Rust]: https://www.rust-lang.org
[`AsRef`]: https://doc.rust-lang.org/std/convert/trait.AsRef.html
[`Cow`]: https://doc.rust-lang.org/std/borrow/enum.Cow.html
