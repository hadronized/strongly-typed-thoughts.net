Lately, I had an interesting talk with colleagues of mine about C++. I told them I needed a safe
`std::unique_ptr` abstraction. I needed such a type because I was handling scarce resources that
needed to be uniquely owned in our code base, but at the same time, checking every time whether
a value is present (`nullptr` being the problem here) yields large numbers of bugs in production.

They asked _“What’s a safest `std::unique_ptr`?”_ My main problem was (still is) that
`std::unique_ptr` can be initialized with `nullptr`. The problem with this is that you cannot
assume that you _always_ own a `T` if you are given a `std::unique_ptr<T>`. References and wrapped
references cannot be used either because we really want to own the data here.

The rest of the discussion is driven by that initial motivation.

As a (mainly) Haskell and Rust developer, I’m also pretty good at writing C and C++ — after all,
I’ve been doing C and C++ since I’m 11 and today, I’m 28; I’ll let you do the math. I especially
developed 3D stuff, ranging from toy experiments to demoscene productions, and lots of other
low-levels things I won’t detail here.

See, I’ve failed a lot while trying things when I was younger — especially around the age of
15 / 16. That’s basically the age where I started to be _very_ frustrated at C++ and, without even
knowing it, was asking to find a _better way to design code_. Yes, I failed a lot, I have no shame
stating it. But to me, failing has been a building block to my experience. The more I fail, the less
likely I’ll fail again the same way. By definition, if you fail a lot on different things… you now
have very good hints and ideas about how _not to fail again_. This is like _learn the hard way_, but
I truly think it’s important, especially to get _why_ something doesn’t work. Reading books and
being told is important, but failing is even more.

Yes, today, with all the hindsight I’ve been through, all the languages I’ve been using – ranging
from C/C++, D, Python, Haskell, Idris, OCaml, Rust, Java, JavaScript, Perl, C#, GLSL and several
others — I think I have a pretty good idea about what’s out there in terms of language ideas.

The goal of this article is to explain _exactly why_ I don’t like that much C++, _why_ I think it
has a lot of bad ideas, and to provide people with material to think and meditate. I truly think
that a good developer needs to call themselves into question regularly and question their process
and established knowledge. I think that, decades ago, C and C++ were the right way to do things.
Today, I think those languages and especially _what has been accepted as ground and established
truth_ about how to do things has become obsolete.

I’m going to explain why in a series of blog articles, starting with this one. Let’s go.

<!-- vim-markdown-toc GFM -->

* [C++ and the wrong idea of constructors](#c-and-the-wrong-idea-of-constructors)
    * [Hidden costs](#hidden-costs)
    * [The C++ broken initialization design](#the-c-broken-initialization-design)
    * [Construction ≠ constructors](#construction--constructors)
* [C++ and its bad standard library](#c-and-its-bad-standard-library)
* [How would you do that in Rust?](#how-would-you-do-that-in-rust)

<!-- vim-markdown-toc -->

# C++ and the wrong idea of constructors

I want to start with a very good example of something that has been accepted as pretty nice when
it was introduced and that is _completely_ obsolete to me. Let’s talk about C++’s _constructors_.
But not _constructors_ by themselves. I want to focus on _how most people_ use them, especially
around the concept of _fallible construction_ — we’re going to talk about exceptions, too!

For those who are not familiar with constructors in C++, let me explain a bit. In C, when you have
a data type (like a `struct`), you can create an object with such a type by allocating memory for it
(either on the stack or the heap) and then initialize its fields. Since C99, you also have
the _designated initializer_ syntax to initialize a `struct` directly while you’re declaring it.
Some examples:

```c
struct Foo {
  int x;
  char const* y;
};

int main() {
  Foo foo;
  foo.x = 3;
  foo.y = "Hello, world!";

  // designated initializer syntax
  Foo foo2 = {
    .x = 3,
    .y = "Hello, world!"
  };
}
```

Even though that code seems pretty _harmless_, to me, it’s very easy to misuse C’s construction
syntax. I will not explain exactly why here because we’re interested in C++, but you’ll understand
what’s the problem by reading on. It relates to being possible to leave a state or part of a state
undefined without having your compiler prevent you from doing that.

## Hidden costs

In C++, you cannot go with any of those way to create a value without a mandatory C++ concept:
_constructors_. Constructors are C++’s way to initialize objects by using the same kind of syntax.
C++ has several constructors:

- _Default constructor_. This one is invoked when you want to create an object without constructing
  it with any parameter. It’s an important constructor in C++, as several containers and standard
  algorithms rely on its presence.
- _Parameterized constructors_: you can create as many parameterized constructors as you want and
  they will take parameters to initialize your objects based on those parameters.
- _Copy constructors_: special constructors called when you want to copy objects. In C++, a
  copy constructor is automatically called when you pass an object `foo` which type is `Foo` to a
  function that expects a `Foo`, for instance.
- _Move constructors_: special constructors called when you _move_ objects around. In C++, a move
  constructor is automatically called when you pass a rvalue reference to something (`Foo &&`).
  You can obtain such references by using `std::move` or `std::forward`.

> There is a slightly exception: [aggregate initialization], which is a bit similar to the C’s
> designated initialization, but requires that you have not declared any constructor – among other
> restrictions — and is then always available, either for public or private code. I will then not
> talk about it. I also have to admit I never completely recall all the rules about POD designated
> initialization.

Let’s demonstrate some of the constructors from above in the following program:

```cpp
#include <iostream>

struct Foo {
  // default constuctor
  Foo() {
    std::cout << "default ctor called" << std::endl;
  }

  // copy constructor
  Foo(Foo const &) {
    std::cout << "copy ctor called" << std::endl;
  }

  // move constructor
  Foo(Foo &&) {
    std::cout << "move ctor called" << std::endl;
  }

  // assignment operator by copy
  Foo & operator=(Foo const &) {
    std::cout << "operator= called by copy" << std::endl;
    return *this;
  }

  // assignment operator by move
  Foo & operator=(Foo &&) {
    std::cout << "operator= called by move" << std::endl;
    return *this;
  }
};

Foo get_foo() {
  return Foo();
}

int main() {
  Foo foo;
  auto foo2 = foo;
  auto foo3 = get_foo();

  return 0;
}
```

In the `main`, what constructors do you think each line is going to call? :) The answer is… it
depends on your compiler. Yes, you heard me right. With my compiler, this is what I get:

- `Foo foo;`: default constructor, which should be the case for every compiler here.
- `auto foo2 = foo;`: copy constructor.
- `auto foo3 = get_foo();`: default constructor. This is due to something called [(N)RVO].

This is already pretty bad, for several reasons:

- It’s hard to tell when we are going to copy something. Think of a more complex data structure,
  such as a `std::vector<Something>`, which might contain thousands of millions of `Something`.
  Because C++ copies by default with the _value semantics_, you might end up with several copies
  while your code doesn’t explicitly copy anything.
- Because copy constructors and move constructors are subject to compiler optimizations, we don’t
  really know when the [(N)RVO] optimization is going to kick in — we can have good confidence
  but we might be wrong. I know some people never really know what `auto foo = something;` does —
  and I feel them, I often spend some time trying to figure out as well.

The existence of copy constructors and the value semantics by default makes C++ hard to reason
about when we are making copies. Other languages, such as Rust, mitigate that risk by using a
move semantics by default. If you want to copy (i.e. clone) something, you have to explicitly ask
for it. For instance, consider:

```cpp
// C++
void foo(std::vector<Foo> foos) {
  // …
}

int main() {
  std::vector<Foo> foos;

  // …

  foo(foos);

  return 0;
```

vs.

```rust
fn foo(foos: Vec<Foo>) {
  // …
}

fn main {
  let foos = Vec::new();

  // …

  foo(foos);
}
```

The C++ program will copy the content of `foos` when calling `foo`. The copy here is due to the
copy constructor of `std::vector` being called. The effect of is that the whole content of the
heap-allocated region owns by the vector will be copied. I typically call that a _deep copy_, or
simply a _clone_.

The Rust program will not make a copy of `foos`. Instead, it will move ownership from the caller
to the calle. `foos` won’t be available anymore in `main` after the call to `foo`. So no heap
allocation will occur here. If we wanted the same behavior as the C++ one, we would do this:

```rust
  foo(foos.clone());
```

That requires that `Foo` implement the `Clone` trait, but it’s off topic here. However, there a
few places where Rust can still do _hidden_ copies that can hurt your performance. By default,
everything is moved. However, if your type implements the `Copy` trait, it means that it can be
safely copied. Such a copy is always performed bit-wise. What’s interesting is that there is, in
theory, nothing different at runtime between _copying_ and _moving_ in Rust. Moving might allow
more optimizations to prevent actually copying data, but imagine you don’t have optimizations.
Both copy and move semantics, in Rust, could be implemented with a `memcpy`. The main difference
is that copying doesn’t lose ownership. So, because the original value, is not moved, can still be
used after copy. Rust has implementations of `Copy` for arrays, which, thus, can yield bad
performances if passed directly to a function, for instance:

```rust
fn bar(_: [u32; 1000000]) {}
```

Calling `bar` will make a copy of the array you pass as argument, which is bad news here. However,
I think it’s pretty unlikely that you allocate such an array on your stack. Most non-primitive
types from the standard library don’t have an implementation for `Copy`, which makes move follow
the move semantics.

Let’s get back to C++, shall we. We’ve established that C++ makes it _very easy_ to write code
which has hidden copies, because of the existence of copy constructors and that value semantics
depends on it. Let’s go on with a problem I have with constructors that is as bad to me.

## The C++ broken initialization design

Imagine that you want to be able to manipulate a kind of integer. That integer cannot be negative
and it cannot be equal to `0`. Also, we want it on 32-bit precision. There are several ways to do
that. Let’s enumerate a few of them (non-exhaustive list):

1. Just use the `int32_t` type from the standard library and every time we try to create or modify
  it, forbid to put values which are less or equal to `0`.
2. Use the `uint32_t` type from the standard library and prevent using `0`. We don’t need to check
  for negative values because `uint32_t` cannot encode them.
3. Create a wrapper type that prevents from constructing values that violate the invariant (the
  invariant being that it cannot be less than `1`.

Obviously, both (1.) and (2.) have a big drawback: you’re going to spend a lot of runtime to check
that the invariant is not violated. Worse, the invariant _leaks in the API_. They have an API
contract flaw: they don’t say to the programmers they cannot be equal to `0`. For instance:

```cpp
void foo(uint32_t non_zero) {
  // …
}
```

Even though the name of the variable is `non_zero`, what prevents a user from calling `foo` with
`0`? Nothing. They can just call `foo(0)` and the code will still compile. They will then get a
really bad runtime undefined behavior, or maybe nothing for several runs and a sudden bug… or you
will have to handle the invariant at every call, which doesn’t seem like a lot of fun — and it’s
dangerous because you can forget to check it.

Instead, we want this:

```cpp
void foo(NonZero non_zero) {
}
```

Here, the function will clearly not be callable with a `uint32_t`. So the provided value _must_ be
statically verified to be non-zero. The `NonZero` wrapper type must then ensure the invariant is
non-violated. I now have a question: do you think `NonZero` has to check the invariant every time
it’s used, or we can do better?

C++ states that `NonZero` should be constructed with a _constructor_, and my point is that
constructors are broken, because not all values can be constructed via C++ constructors. But they
are not broken the way you think. In theory, and actually, there is a way to use C++ constructors
in sound ways. Try to spend a few minutes and think about it. Can you implement a `struct NonZero`
with an API exposing **only constructors** to create and initialize non-zero values? There are two
possibilities:

- Either you, indeed, use a constructor, but because you want to be able to prevent people from
  building non-zero values, you will have to _fail_ in your constructor. And failure in constructors
  is possible by throwing an _exception_. I don’t accept that answer because exceptions should
  **only be used in exceptional failure situations**, and this is not one of them. I’ll explain
  why.
- By still allowing construction of the object but putting it in an invalid state. I consider this
  a non-answer as well, as you will have to keep track of the invariant every time you want to use
  the `NonZero`, and because it’s very easy to build a `NonZero` with a violated invariant with your
  code compiled correctly: `auto foo = NonZero(0);`. So, it’s a _no_.

I want to be very very crystal clear here. Exceptions in C++ are heavy. They require unwinding most
of the time when some code throws them. They are needed to handle exceptional errors, such as an
_out of memory_ error, or a dead thread. People have been abusing exceptions for too long. Really.
Stop using exceptions. Calling `NonZero(x)` with `x` being `0` is an error that originate from
several places, because `NonZero` is a pretty abstract concept. You shouldn’t assume it’s
exceptional that someone tries to build one with `0`. It’s part of its API: it must fail to
construct if built with `0`. The API must convey the fact it can handle that kind of error. So
it’s an error that is covered by the scope of the type and its carried invariant.

An example of exceptional error here would be that, when you construct your `NonZero`, you’re out
of space on the stack. Throwing an error here seems okay to me. The problem, here, is not that we
have a constuctor that fails. The problem is that we used a constructor to encode fallible
construction. That’s the problem. My point here is that constructors must **not** be used for
fallible constructions.

So… what are we left with? If you really want to _only_ use constructors without exceptions,
you have no other choices but to accept to have a dangling invariant. The invariant here is that
the wrapped `uint32_t` must not be `0`.

> A small note on invariants: they must be held _before_ and _after_ you call a public function on
> your type. We can violate them inside our private code for performance and optimization purposes,
> but don’t forget that you must rebalance everything so that the invariant is held when you give
> control back to the user. This is especially **hard** to do in C++ because of exception safety, so
> a rule of thumb: try to never violate invariants, even in private code.

```cpp
struct NonZero {
  // prevent people from creating default non-zero values; it makes no sense
  NonZero() = delete;

  NonZero(uint32_t value): _wrapped(value) {
    // that would break your application at runtime on debug and do nothing on release… :(
    assert(_wrapped != 0);
  }

  // let C++ automatically implement some stuff for us, which won’t break the invariant
  NonZero(NonZero const &) = default;
  NonZero(NonZero &&) = default;

  NonZero & operator=(NonZero const &) = default;
  NonZero & operator=(NonZero &&) = default;

private:
  // the invariant must be held on this value
  uint32_t _wrapped;
};
```

As you can see, having a `NonZero` with this definition doesn’t mean the value cannot be `0`.
Example:

```cpp
int main() {
  auto nz = NonZero(0); // meh
  return 0;
}
```

This code compiles fine and will either abort at runtime on an assert or just silently do something
very wrong on release code.

So what can we do to this situation?

## Construction ≠ constructors

The thing is that, if you’ve only done C++ in your life, you’re likely to think that the best thing
to do is to give up and use exceptions. After all, they’re pretty nice.

```cpp
#include <stdexcept> // you’ll need that for the exception type

struct NonZero {
  // prevent people from creating default non-zero values; it makes no sense
  NonZero() = delete;

  NonZero(uint32_t value): _wrapped(value) {
    if (_wrapped == 0) {
      throw std::invalid_argument("cannot create a NonZery with 0");
    }
  }

  // let C++ automatically implement some stuff for us, which won’t break the invariant
  NonZero(NonZero const &) = default;
  NonZero(NonZero &&) = default;

  NonZero & operator=(NonZero const &) = default;
  NonZero & operator=(NonZero &&) = default;

private:
  // the invariant must be held on this value
  uint32_t _wrapped;
};
```

Now, when you call `NonZero(0)`, an exception is thrown. You can catch it with a `try` / `catch`
block. But please wait a minute. Several points:

- Again, exceptions should **not** be used for such errors. I get it you’ve given up on that rule,
  but you shouldn’t. Save exceptions for exceptional cases. This is _not_ exceptional. This is
  [naaaaaaaaaaht].
- When you look at the constructor definition, nothing tells you it can fail. You have to read the
  body of the constructor to check for `throw` — and again, it could quickly get hard because you
  might call functions that throw too…

That last point is what makes C++ a very hard language what it comes to error handling to me.
Error handling, most of the time, is based on exceptions, which are invisible at the type-level.
I got some remarks stating that, nowadays, modern C++ code bases use the `noexcept` keyword when
something cannot fail, and the rest of the time, we must assume something can fail. Okay, but
then, fail _how_? What kind of error? How do I know that? Rely on whether it’s written in a
Doxygen documentation, that, most of the time, doesn’t even exist? Also, just have a look at how
people handle failures in C++. Do you see those `try ` / `catch` _everywhere_ `noexcept` is not
annotated? No, because people don’t use `noexcept` and when they use library, they don’t even
apply that advice to themselves either. The problem with exceptions not being visible in types and
function signatures is that you have to read the code (or the missing Doxygen documentation :)) to
_find out_ what exceptions you need to catch. The other problem is that they can be ignored and
implicitely and automatically propagated upwards in the call stack. There is no way to force a
user to either handle the error or explicitly pass it to their caller.

All that to say: don’t do this. Don’t think constructors are a good idea to construct types in all
possible situations. Failible constructions shouldn’t be done with public constructors and
exceptions. However, it doesn’t mean constructors cannot be used to implement fallible constructions.
That seems tricky, and you’re right. You might wonder:

> _“Okay okay, you’ve advanced points but… still, how would you do it?”_

My implementation — and today, I would only accept that one — to this problem is this: since we
cannot create a value without using a constructor, and since constructors fail to encode fallible
constructions without using exceptions — which I don’t accept either, I’m going to use a private
constructor to build my `NonZero` and _not_ check the invariant. Then, using a `static` method, I
can return a more typed object to encode a possible failure, by checking the invariant _before_
creating the object.

Consider:

```cpp
#include <optional>

struct NonZero {
  // prevent people from creating default non-zero values; it makes no sense
  NonZero() = delete;

  // let C++ automatically implement some stuff for us, which won’t break the invariant
  NonZero(NonZero const &) = default;
  NonZero(NonZero &&) = default;

  NonZero & operator=(NonZero const &) = default;
  NonZero & operator=(NonZero &&) = default;

  // static method used to create a NonZero; the only way to create one
  // using the public interface
  std::optional<NonZero> from_u32(uint32_t value) {
    std::optional<NonZero> r;

    if (value != 0) {
      // call to the private ctor; the invariant is already checked in this branch
      r = NonZero(value);
    }

    return r;
  }

private:
  // private constructor that doesn’t check the invariant
  NonZero(uint32_t value): _wrapped(value) {}

  // the invariant must be held on this value
  uint32_t _wrapped;
};
```

So let me explain a bit all this code. First, the `NonZero(uint32_t)` constructor is private. It
has to be private because it doesn’t enforce the invariant. It allows to construct any `NonZero`.
We need it because C++ requires you to use a constructor to create a value of type `NonZero`.
Because I don’t want exceptions, I just don’t check the invariant here so that constructor
cannot fail.

The `static` method called `from_u32` is the only entry-point to create a value of type
`NonZero`. As you can see, it returns a `std::optional<NonZero>`, which means that it can fail.
If you call that function with `0`, you will get an empty optional value. That function is
implemented by creating an optional value, allocated on the stack with the default constructor —
which makes it empty. Then, we check the invariant and if it’s not violated, we allocate and
initialize the `NonZero`, and return the whole thing.

# C++ and its bad standard library

For the rest of the article, consider we add the following method to `NonZero`:

```cpp
  uint32_t value() const {
    return _wrapped;
  }
```

If you’ve followed carefully what we’ve been doing here, a call to `NonZero::value()` will _never_
return `0`, because construction statically disallows building such non-zero values. You then don’t
have to check for it!

However, we’re not completely done. We’ve used the `std::optional` standard type. How are we
supposed to use that type? Looking at the official documentation, we get use the [`operator*` or
`operator->`](https://en.cppreference.com/w/cpp/utility/optional/operator*) to access the
underlying object. We have the
[`has_value()`](https://en.cppreference.com/w/cpp/utility/optional/operator_bool) method to check
whether a value is present.

```cpp
auto nz = NonZero::from_u32(0);

if (nz.has_value()) {
  std::cout << "We have a value! " << nz->value() << std::endl;
}
```

That seems nice, right? Well, not really. C++ doesn’t have _exhaustive pattern matching_, which
makes it impossible to statically ensure you exhaustively check a `std::optional`. What it means
is that, given the current contract based on `has_value()` and the dereference operators, we
can still break our program and bring _undefined behaviors_ without having our compiler complain.

```cpp
auto nz = NonZero::from_u32(0);

std::cout << "We have a value… right? " << nz->value() << std::endl;
```

That program is perfectly fine and valid. However it’s not. It’s completely wrong, because
`std::optional`’s API allows developers to access a value that might not be there.

This is the same situation as doing this:

```cpp
int * ptr = nullptr;
int x = *ptr;
```

Statically fine. Dynamically pretty bad, right? C++’s standard library’s `std::optional` could
have been done in a much safer way, by, for instance, providing you with some combinators to
work with the underlying type. C++ doesn’t have _exhaustive pattern matching_ but it still has
higher-order functions. We could then have something like:

```cpp
auto nz = NonZero::from_u32(0);

nz.maybe(
  // called if no value is present
  []() {
    std::cout << "No value there." << std::endl;
  },
  // called if a value is present
  [](NonZero const &v) {
    std::cout << "We have a value: " << v.value() << std::endl;
  }
);
```

For most C++ developers, that code will look like very verbose and wrong, and I agree. Without
pattern matching, it’s very hard to safely and statically use a `std::optional`.

# How would you do that in Rust?

In Rust, we have _exhaustive static dispatch_. `std::optional` is called `Option` and Rust
doesn’t have constructors _at all_.

Values can be constructed by providing _all_ fields, if they are all available (i.e. that’s
always the case in the module the type is defined in; for the rest, you need access to the fields
by marking them all `pub`).

This is my `NonZero` wrapper in Rust:

```rust
struct NonZero {
  // we don’t make that value pub so no one can directly construct a NonZero
  // outside of our module
  wrapped: u32,
}

impl NonZero {
  // yes, new is nothing special in Rust, so we can use it as a method!
  // Self references the current type we’re adding an impl for (here, NonZero)
  pub new(wrapped: u32) -> Option<Self> {
    if wrapped == 0 {
      None
    } else {
      Some(NonZery { wrapped })
    }
  }
}

fn main() {
  match NonZero::new(0) {
    Some(value) => println!("we have a value! {}", value),
    None => println!("we don’t have a value… :("),
  }
}
```

That’s all. The program is statically safe, as the compiler can make sure we’re correctly using
the `Option<NonZero>`. We could use combinators that do the pattern matching for us. For
instance, imagine that you want the value `8080` if it’s not correctly constructed, or what it
contains otherwise:

```rust
let port = nz.unwrap_or(8080);
```

Here, the type of `port` is `NonZero`. `Option::unwrap_or` might be implemented like this:

```rust
impl<T> Option<T> {
  pub fn unwrap_or(self, default_value: T) -> T {
    match self {
      Some(v) => v,
      None => default_value,
    }
  }
}
```

There are a lot of similar combinators, like `Option::map`, `Option::or`, `Option::unwrap_or_else`,
etc. I’ll let you dig by yourselves. However, there is an important point to make here. In C++,
there is the `std::optional::value()` method that returns the value if it’s present or just throws
an error (which I dislike). In Rust, there is the `std::Option::unwrap()` method, that returns the
value if it’s present or _panics_ otherwise… which I also dislike. `unwrap()` can be dangerous in
Rust and I think it should be marked `unsafe`. But a more detailed article is needed about why
I want it to be marked `unsafe` and it’s off topic anyways.

So, we’re hitting the end of the article. It might be a lot of information; sorry. To sum up, in
C++:

- Construction must not be assumed to be done exclusively via constructors.
- _Pure_ construction (as in infallible) can use constructors if you want.
- _Fallible construction_ can use:
  - A static function that returns a typed object via, for instance, `std::optional` or similar,
    and enforce invariants / pre-conditions.
  - A private constructor to build the result value _after_ that invariant and pre-conditions get
    checked.
- We see that we can make C++ much easier to reason about by following those rules. It will not be
  as handy as in Rust where no constructor are needed, but it’s still a good improvement to me.

Next time, I’ll bring you on a tour with me around the concept of _inclusion polymorphism_, which
is most of the time called _inheritance_ in the world of OOP languages. But we’ll still stick to
C++. ;)

[Keep the vibes!](https://phaazon.net/media/uploads/the_golden_smile.gif)

> [Discussion on the article here.](https://www.reddit.com/r/rust/comments/f8j304/lets_talk_about_c_constructors)

[aggregate initialization]: https://en.cppreference.com/w/cpp/language/aggregate_initialization
[(N)RVO]: https://en.cppreference.com/w/cpp/language/copy_elision
[naaaaaaaaaaht]: https://phaazon.net/media/uploads/what_a_story_mark.gif
