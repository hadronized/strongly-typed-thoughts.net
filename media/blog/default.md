# Don’t use Default

## Background knowledge: typeclasses and laws

In **Haskell**, we have a lot of
[typeclasses](https://en.wikipedia.org/wiki/Type_class). Those are very handy
and – in general – come with laws. Laws are very important and give hints on how
we are supposed to use a *typeclass*.

For instance, the `Semigroup` *typeclass* exposes an operator (`(<>)`) and has
an **associativity** law. If `a`, `b` and `c` have the same type `T`, if we know
that `T` is a `Semigroup`, we have the following law:

    a <> b <> c = (a <> b) <> c = a <> (b <> c)

If `T` is a `Monoid`, which is a `Semigroup` with an identity (called `mempty`),
we have a law for *monoids*:

    a <> mempty = mempty <> a = a

Those laws can – *have* – to be used in our code base to take advantage over the
structures, optimize or avoid boilerplate.

## The Default typeclass

In some situations, we want a way to express default values. That’s especially
true in OO languages, like in the following **C++** function signature:

    void foo(float i = 1);

In **Haskell**, we cannot do that, because we have to pass all arguments to a
function. The following doesn’t exist:

    foo :: Float -> IO ()
    foo (i = 1) = -- […]

And there’s more. Even in C++, how do you handle the case when you have several
arguments and want only the first one to be defaulted? You **cannot**.

So, so… Some **Haskellers** decided to solve that problem with a *typeclass*.
After all, we can define the following typeclass:

```haskell
class Default a where
  def :: a
```

We can then implement `Default` and have a *default* value for a given type.

```haskell
instance Default Radians where
  def = Radians $ 2*pi

instance Default Fullscreen where
  def = Fullscreen False
```

However, there is an issue with that. You cannot use `Default` without creating
`newtype`s to overload them. Why? Well, consider the following `Default`
instance:

```haskell
instance Default Float where
  def = -- what should we put here?
```

Remember that, in **Haskell**, an instance is defined only once and is
automatically imported when you import the module holding it. That means you
cannot have the following:

```haskell
-- in a module A
instance Default Float where
  def = 0

-- in a module B
instance Default Float where
  def = 1

-- in a module C
import A
import B

-- What instances should we use?
```

> *Hey, that’s easy. We just have to keep the modules apart, and import the one
we want to use!*

Yeah, well. No. No.

![](http://phaazon.net/pub/god_please_no.gif)

Orphan instances are **wrong**. You should read
[this](https://wiki.haskell.org/Multiple_instances) for further explanations.

That’s why we have to use `newtype`s everywhere. And that’s boring. Writing code
should always have a goal. When we write as much boilerplate code as real code,
we can start thinking there’s something wrong. Worse, if we have more
boilerplate than real code, well, something is terribly wrong. In our case,
we’re introducing a lot of `newtype`s for only being able to use `def` at a few
spots. Is that even worth it? Of course not.

## Default is evil

The `Default` typeclass is evil. It’s shipped with default instances, like one
for `[a]`, which defaults to the empty list – `[]`. It might be clear for you
but why would I want to default to the empty list? Why not to `[0]`? Or a more
complex list? I really doubt someone ever uses `def :: [a]`.

Another reason why `Default` is wrong? There’s absolutely **no law**. You just
have a default value, and that’s all.

```haskell
bar :: (Default a) => a -> Maybe String
```

Can you say what the default is for? Of course you cannot. Because there’s no
law. The instance has no real meaning. A default value makes sense only for the
computation using it. For instance, the empty list makes sense if we glue it to
the *list concatenation*.

## We already have a lot of defaults

In [base](https://hackage.haskell.org/package/base), there’re already several
ways to express *defaulted values*.

The `Monoid`’s `mempty` is a way to express a default value regarding its
binary operation (`(<>)`).

The `Alternative`’s `empty` provides a similar default, but for first-class
types.

The `MonadZero`’s `mzero` provides a different default, used to *absorb*
everything. That’s a law of `MonadZero`:

    mzero >>= f = mzero
    a >> mzero  = mzero

Those defaults are interesting because we can reason about. If I see `mzero` in
a *monadic code*, I know that whatever is following will be discarded.

# Conclusion

So please. Stop using `Default`. It doesn’t bring any sense to your codebase. It
actually removes some! If you want to provide functions with defaulted
arguments, consider partially applied functions.

![](http://phaazon.net/pub/default_stats.png)

[data-default](https://hackage.haskell.org/package/data-default) is a very
famous package – look at the downloads! You can now have some hindsight about it
before downloading it and ruining your design. ;)

Happy hacking. :)
