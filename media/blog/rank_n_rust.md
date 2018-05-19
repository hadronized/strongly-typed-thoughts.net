Yesterday, I came across a very simple problem [that I summed up here](https://www.reddit.com/r/rust/comments/8kh94b/is_there_any_way_to_have_move_semantics_rankn).
The idea is the following:

  - You want to implement a unary function `foo` that takes a function `F` as sole argument and
    returns its result.
  - `F` is a unary function that takes a function `Λ` as sole argument.
  - `Λ` doesn’t take any argument and return a type `T`.
  - We want linear typing here, but because it would make the blog post more complicated, we’ll
    ignore that property.

In **Haskell**, you would represent this scenario like this:

```
data T = T -- we’re not interested in this type yet

               v this is F
        --------------
foo :: ((() -> T) -> b) -> b
         -------
            ^ this is Λ

-- or for people less familiar with Haskell, here’s a decomposed version (but still the same thing)
type Λ = () -> T
type F b :: Λ -> b
foo :: F b -> b
```

> Note: it’s pretty much known in the Haskell world but maybe not in others: if you have a value,
> like `a`, it’s isomorphic to a unary function that returns this value and takes `()` as single
> argument – `() -> a`.

> Note²: for curious, in Haskell, the syntax `a ->. b` is a linear arrow. More about this on
> [linear Haskell]. I didn’t include the syntax in the snippet because it would make the post too
> complicated for our purpose.

> Note³: this is a rank-2 type function, because it’s a function that takes a function (1) that
> takes another function (2). There’re levels of “nesting” there.

In **Rust**, argh… **Rust** is not as powerful as **Haskell** for complex abstractions – even though
we’re working on making it better and better. To me, **Rust** competes with **Haskell**, but the
current blog post will show you that it still has a lot to learn from its parent! :)

Let’s sketch something in **Rust**.

```
struct T; // we’re not interested in this type yet

fn foo<F, B>(f: F) -> B where F: FnOnce(???) -> B {
  f(|| 42)
}
```

What should we put in place of the `???` placeholder? Clearly, looking at the **Haskell** snippet,
we want a function that produces a `T`. Something like that:

```
fn lambda() -> T
```

So we could use the type `fn () -> T`, but that will not work, because a lambda has not such a
type – you can see the lambda `|| 42`. Let’s try something else.

```
fn foo<F, B>(f: F) -> B where F: FnOnce(impl FnOnce() -> T) -> B
```

If you look at the [impl Trait RFC](https://github.com/rust-lang/rfcs/blob/master/text/1522-conservative-impl-trait.md),
you see that it’s a **Rust** construct to build a contract that lets you handle a value through
*traits* without knowing the type – we call that an *existential*. The type is picked by the
*callee*, which in our case is the function itself.

However, this syntax doesn’t work because – as for now – `impl Trait` works only in return position
and the consensus seems to have agreed that when `impl Trait` will be implemented in argument
position, **it will not mean an existential, but a universal**, e.g.:

```
fn bar<I: Iterator<Item = u32>>(i: I)
fn bar<I>(i: I) where I: Iterator<Item = u32>
fn bar(i: impl Iterator<Item = u32>)
```

Are all the same thing.

> Note: I’ve made a ranting about the decision to give universal semantics to `impl Trait` in
> argument position [here](https://github.com/rust-lang/rust/issues/34511#issuecomment-390358483).
> I don’t want to start a type theory war, if it’s the consensus, then we have to accept it and
> move on. It’s just a bit a pity. Topic closed, let’s get back to our sheep!

So we cannot use that. Argh! Let’s try something else then!

```
fn foo<F, G, B>(f: F) -> B where F: FnOnce(G) -> B, G: FnOnce() -> T
```

This cannot work because here you have two universals `F` and `G`, meaning that `G` will be picked
by the caller while you want to decide of its type directly in your function (remember the lambda).

```
fn foo<F, B>(f: F) -> B where F: for<G: FnOnce() -> T> FnOnce(G) -> B
```

I like this one *a lot*. It uses a concept called [HRTB] in **Rust** which you can see by the
`for<_>` syntax. However, this will not work because as for today, [HRTB] only works with lifetimes.

> Ok, that seems a bit desperate!

Hm, how about this:

```
fn foo<F, B>(f: F) -> B where F: FnOnce(Box<FnOnce() -> T) -> B
```

Even though that seems to typecheck, you won’t ever be able to use `Λ` here because you cannot move
out of a `Box` with `FnOnce`. This limitation is removed with the `FnBox` type:

```
fn foo<F, B>(f: F) -> B where F: FnOnce(FnBox() -> T) -> B
```

However:

  - `FnBox` is unstable and will only work on nightly right now.
  - It *boxes* the function, which is not what we want there.

> Ok, that seems desperate. Period.

Well, if we only stick to those tools (i.e. functions, type variables and trait bounds), currently,
yeah, there’s no solution. However, if we add the traits to our toys, we can work around the
problem:

```
trait GConsumer {
  type Output;

  fn call_once<G>(self, f: G) -> Self::Output where G: FnOnce() -> i32;
}

fn foo<F>(f: F) -> F::Output where F: GConsumer {
  f.call_once(|| 32) // hurray!
}
```

This snippet is actually quite interesting. We introduce the *existential* via a trait. The
existential is encoded with the `G` type variable of the `call_once` method of `GConsumer`. If you
look at that function from `foo` perspective, `G` is definitely an *existential*! – and it’s a
*universal* for `call_once`.

I really like this idea, because it’s simple and legacy **Rust**. But I feel a bit confused, because
it’s simple to define but hard to use. Instead of just passing a closure to `foo`, people will now
have to implement a separate type on which will be called `call_once`. That’s a lot of boilerplate.

I hope we’ll come to a better solution to this problem – if you can do it with any trait, it should
be possible with `FnOnce` and a few candies into the **Rust** language itself.

That’s all for me for today. Keep the vibes and have a nice weekend!

[linear Haskell]: https://ghc.haskell.org/trac/ghc/wiki/LinearTypes
[HRTB]: https://doc.rust-lang.org/nomicon/hrtb.html
