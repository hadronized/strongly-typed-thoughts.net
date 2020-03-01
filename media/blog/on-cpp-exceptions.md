> This article is a sequel to [Let’s talk about C++ constructors](https://phaazon.net/blog/c++-constructors).
> Even though not required, reading that article first might give you more information on the motivation and
> my state of mind regarding C++ and software development.

This time, I want to talk about C++ exceptions. But talking about exceptions for the sake of talking about
them has very little value. Let’s instead talk about error handling.

# Infallible pure functions

Imagine a function that takes some input, like a `String`, and outputs something else, like the length of the
string. Such a function is _pure_: its output depends _only_ on its arguments. It cannot have any side-effect
nor return something else. It is also infallible as it can _always_ return a valid length for all possible
values it can be called with. Imagine the following C++ implementation:

```cpp
size_t get_len(std::string const & s) {
  return s.length();
}
```

> Even though C++ doesn’t have the concept of purity, let’s assume we can express the concept, just because
> it’s a powerful concept that we can apply to any language. The compiler will not know about it, but the way
> we design things does.

That function is pure and infallible. When you look at the signature, you see that it takes a read-only
reference on a `std::string` , and returns a `size_t`. And yes, I know about `noexcept`. However, this
specifier cannot be trusted. Consider:

```cpp
#include <string>

void foo() {
  throw std::runtime_error("That should be seen by the compiler, right?");
}

size_t get_len(std::string const & s) noexcept {
  foo();
  return s.length();
}

int main() {
  std::string foo = "foo";
  get_len(foo);
  return 0;
}
```

Compile with (I’m running Archlinux):

```
g++ -std=c++2a -Wall -pedantic a.cpp
```

No compiler warning. Run it:

```
terminate called after throwing an instance of 'std::runtime_error'
  what():  That should be seen by the compiler, right?
zsh: abort (core dumped)  ./a.out
```

What that little snippet tells us is that even though `get_len` is annotated with `noexcept`… it can still
throw. Not directly in its body, but functions it calls may throw. When I was introduced to that keyword,
years ago, I was suspicious. Since C++ will throw exceptions for — erm — exceptional errors, such as
_out of memory_ errors, then… even a `noexcept` function can still throw errors. Then, because of that,
`noexcept` cannot propagate downwards in your call stack. If `A` is `noexcept` and `B` _may throw_, then
calling `B` from `A` is valid in C++.

`noexcept` is just a documentation keyword and an opportunity for your compiler to optimize your code. Your
compiler can only emit warnings if you use the `throw` keyword directly in the body of the function that is
marked `noexcept`. Also, it’s important to notice that, given the team you work in, or the project you work
on, it’s possible to see the use of `noexcept`… like not at all. It’s all about convention; your compiler
will not enforce that… which is a problem to me. It’s a problem because more freedom means more opportunities
for people to make mistake. To forget about annotating a function with `noexcept`. Or, worse, it gives
opportunities to people who just don’t care and want to rush, making reviewing their code more challenging
than needed.

# Fallible functions

In my previous article, I’ve been criticized for not explaining enough what I mean about _exceptional
errors_ and that it was a highly subjective point. I’ll try to explain more in this section.

Imagine that you want to implement a function that will perform a lookup. If it finds the key you give
as argument, it will return the associated object. However, what to do when the key is not there? If you read
a bit my previous article, you know that I would use a sum type to encode the error in the type system. But
let’s do it _the C++ way_. Let’s use exceptions.

```cpp
Object lookup(Key const & key);
```

If you look at that signature, you’ll notice an important point. There is no error handling annotation. Most
of the time, people will follow some guidelines to put that information in the documentation directly.
However, several points:

- What happens if a teammate of yours or even yourself – after some weeks / months – forgets about that
  documentation line?
- Because you might call that function from _anywhere_ — even in `noexcept` code, as demonstrated in the
  previous section — how do you know, when reading the code, that a call to this function can throw?
- The last point is especially true when refactoring. Imagine that this function belongs to a block of code
  delimited with a `try catch` block. Do you assume the whole block as atomic? If so, do you lookup for the
  documentation of _all of the functions_ called in that block? What happens if you move that function out
  of the block?

Now, that just assumes a flat block. But it’s easy to guess that you will have to do that for the whole stack
of functions being called — i.e. as soon as you find a `noexcept` function in the stack… well nothing, you
have to go on, since a `throw` might be hidden in a function deeper in the stack.

Most of the time, the replies I get about that topic are, either:

- **_“Just read the documentation”_**. That argument completely ignores the last part of my point above — i.e. do you
  _really_ read the documentation of _all_ the functions being directly or transitively called in a
  function? That seems insane. Also, you might argue that the documentation can say, at a function level `N`,
  that there is a throw at level `N - k`. However, that seems like an impossible task to maintain. You might
  forget to update the documentation if you stop throwing that exception or throw another object with a
  different type, etc.
- **_“Use better names. If you use a function that, in its name, expresses the idea that it might fail, it’s
  easier to refactor / use the function correctly”_**. That is true, even though you will _always_ find someone
  abusing it and using it without a `try catch` block.
- **_“We don’t care about handling errors: we will just handle them at the top-level of the program / function
  call stack with a single try catch block”_**. That argument doesn’t survive five seconds as soon as you
  talk about, for instance, serialization or map lookups.

About the documentation and naming… it adds another problem: humans. We are fallible. You might work on a
project that doesn’t document correctly. Or that doesn’t even have proper convention. Or several ones. When
considering exceptions for error handling, I think it’s important to imagine what will happen in X months.
After the codebase has become complex, large, with a lot of edge cases and possible errors. Maintainability
should be a goal. No one enjoys having to read through the bodies of twenty functions to understand why their
program crashed or why the GUI displays a pop-up with the content of an exception.

The sooner you can see an error, the better. If that _sooner_ can be “compile-time”… why would you want to
still push the error to the runtime? There are things I will never understand.

On the other side, consider sum types:

```cpp
std::optional<Object> lookup(Key const & key);
```

Even though it’s still pretty bad to me because of how `std::optional` is made (have a look at my previous
article), it has the advantage of being typed. No documentation is needed — but please do document to
explain what can fail though — and your compiler can safely prevent you from doing bad things. Of course,
this is limited by how you use the `std::optional`, as C++ doesn’t have pattern-matching. But I would like
to reply to an argument I hear every now and then: **it’s not because a better solution is not perfect that
it should be discarded to stay on your legacy solution**. Imagine that you have a tool `X` with several
issues, `{a, b, c, d}`. Now imagine we suggest to switch to a new tool, `Y`, with issues `{a, d}` only.
Yes, you still have two issues… but you have less. In the case of exceptions vs. a type-system, in the
case of C++, yes, you can still call `.value()` on an empty `std::optional` and crash your program. But
you don’t have the problem of hidden and untracked error handling. You can simply use exceptions for
exceptional cases. Those cases that are _not_ function boundaries nor edge cases. And yes, I do think that
most of the standard C++ exceptions, such as `std::invalid_argument`, are to be completely avoided.

But here, exceptions have an advantage if we stop there: they provide an error description.

# Fully-typed failures

Fixing that problem is pretty trivial with a strong type-system and
[algebraic data types](https://en.wikipedia.org/wiki/Algebraic_data_type). We want to use those to create
a _result_ type, that can either be something, or a typed error, that would contain exactly the same
information you have in a regular exception.

C++ doesn’t really have that out of the box but it could be made, in the same way `std::variant` exists.
Imagine a hypothetical `std::either` type and let’s implement a function that parses something:

```cpp
std::either<Something, ParseError> parse(std::string const & input);
```

That function returns _either_ a successful object (`Something`) or an error (`ParseError`).

With that signature, it’s clear that the function can fail with `ParseError`. The point is that the caller
_must_ do something about it. If they don’t know what to do with it — imagine a parser combinator or some
code that doesn’t know how to handle a parse error and _requires_ its caller to handle the error — then
the function needs to abort and propagate the error upwards. That looks like a bit like the interface you
have with exceptions… but here, the interface is visible at the type-level.

Obviously, you cannot use `throw` to propagate upwards. You need to use `return` from your function. With
either a macro or some language construct, C++ could make that propagation smoother, but currently, it
doesn’t have a proper way to do it. So we’d be left with macros only, or manual propagation. Since C++
doesn’t have pattern-matching nor exhaustive patterns, it would be pretty hard to implement that mechanism
in a complete sound way. As with `std::optional`, it’s not perfect, but it would be slightly better than
using opaque exceptions.

One final point. Sometimes, I wonder what it would be like to just give up on my ideas of using a strong
type system in C++. The language is using exceptions and people are used to it. That’s _the C++ way_. So…
why not changing the way exceptions work so that they’re not opaque, and propagate upwards? I remember
the `throw` specifier, used to state what a function might throw. But again, it’s not enforced by compiler.
Worse, it’s being deprecated in C++ 2020.

I voluntarily omitted any reference to either Haskell, Rust or any language like that so that people don’t
think I’m trying to compare C++ to another language. I’m having a look on C++ after almost two decades using
it and what else I’ve learned. The situation is, to me, frustrating. Because yes, whatever the good arguments
against exceptions, people still use them and error handling in C++ is still about exceptions. So you still
can have constructors that fail. You still depend a lot on documentation and your compiler cannot tell you
when something is not okay. We are all fallible, way more than we think.
