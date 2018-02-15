Hi! It’s been a while I haven’t posted something here. I haven’t been around for a few weeks and
I’ve been missing writing here. I didn’t work that much on
[luminance](https://www.stackage.org/package/luminance) or other projects, even though I still
provided updates for stackage for my packages. I worked a bit on
[cheddar](https://github.com/phaazon/cheddar) and I hope to be able to release it soon!

Although I didn’t add things nor write code at home, I thought a lot about graphics API designs.

# Graphics API designs

## Pure API

APIs such as [lambdacube](http://www.lambdacube3d.com/) or
[GPipe](https://hackage.haskell.org/package/GPipe) are known to be graphics *pure API*. That means
you don’t have use functions bound to `IO`. You use some kind of
[free monads](http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html) or
an [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree) to represent the code that will run on
the target GPU. That pure design brings numerous advantages to us:

- it’s possible to write the GPU code in a declarative, free and elegant way;
- because of not being `IO`-bound, side-effects are reduced, which is good and improves the code
  safety;
- one can write GPU code and several interpreters or/and with different technologies, hence a
  loosely coupled graphics API;
- we could even imagine serializing GPU code to send it through network, store it in a file or
  whatever.

Those advantages are nice, but there are also drawbacks:

- acquiring resources might not be explicit anymore and none knows exactly when sparce resources
  will be loaded into memory; even though we could use *warmup* to solve that, *warmup* doesn’t
	help much with dynamic scene where some resources are loaded only if the context enables it (like
	the player being in a given area of the map, in a game, for instance);
- interfacing! people will have to learn how to use free monads, AST and all that stuff and how to
  interface it with their own code (*FRP*, render loops, etc.);
- performance; even though it should be okay, you’ll still hit a performance overhead by using pure
  graphics frameworks, because of internals mechanisms used to make it work.

So yeah, a pure graphics framework is very neat, but keep in mind there’s – so far – no proof it
actually works, scales nor is usable for a decent high-performance for end-users. It’s the same
dilemna as with Conal’s *FRP*: it’s nice, but we don’t really know whether it works *“at large scale
and in the real world”*.

## IO-bound API

Most of the API out there are IO-bound. *OpenGL* is a famous C API known to be one of the worst one
on the level of side-effects and global mutations. Trust me, it’s truly wrong. However, the pure
API as mentioned above are based on those impure IO-bound APIs. So we couldn’t do much without them.

There are side effects that are not that bad. For instance, in OpenGL, creating a new buffer is a
side-effect: it requires that the CPU tell the GPU *“Hey buddy, please create a buffer with that
data, and please give me back a handle to it!”*. Then the GPU would reply *”No problem pal,
here’s your handle!”*. This side-effect don’t harm anyone, so we shouldn’t worry about it too much.

However, there are nasty side-effects, like binding resources to the OpenGL context.

So what are advantages of IO-bound designs? Well:

- simple: indeed, you have handles and side-effects and you have a (too) fine control of the
  instruction flow;
- performance, because `IO` is the naked real-world monad;
- because `IO` is the high-order kinded type of any application (think of the `main` function),
  an `IO` API is simple to use in any kind of application;
- we can use `(MonadIO m) => m` to add extra flexibility and create interesting constraints.

And drawbacks:

- `IO` is very opaque and is not referentially transparent;
- `IO` is a dangerous type in which no one has no warranty about what’s going on;
- one can fuck up everything if they aren’t careful;
- safety is not enforced as in pure code.

## What about luminance’s design?

Since the beginning, luminance has been an API built to be simple, *type-safe* and *stateless*.

*Type-safe* means that all objects you use belong to different type sets and cannot be mixed between
each other implicitely – you have to use explicit functions to do so, and it has to be meaningful.
For instance, you cannot create a buffer and state that the returned handle is a texture: the type
system forbids it, while in OpenGL, almost all objects are in the `GLuint` set. It’s very
confusing and you might end up passing a texture (`GLuint`) to a function expecting a framebuffer
(`GLuint`). Pretty bad right?

*Stateless* means that luminance has no state. You don’t have a huge context you have to bind stuff
against to make it work. Everything is stored in the objects you use directly and specific context
operations are translated into a different workflow so that performance are not destroyed – for
instance luminance uses batch rendering so that it performs smart resource bindings.

Lately, I’ve been thinking of all of that. Either turn the API pure or leave it the way it is. I
started to implement a pure API using *self-recursion*. The idea is actually simple. Imagine this
`GPU` type and the `once` function:

```haskell
import Control.Arrow ( (***), (&&&) )
import Data.Function ( fix )

newtype GPU f a = GPU { runGPU :: f (a,GPU f a) }

instance (Functor f) => Functor (GPU f) where
  fmap f = GPU . fmap (f *** fmap f) . runGPU

instance (Applicative f) => Applicative (GPU f) where
  pure x = fix $ \g -> GPU $ pure (x,g)
  f <*> a = GPU $ (\(f',fn) (a',an) -> (f' a',fn <*> an)) <$> runGPU f <*> runGPU a

instance (Monad m) => Monad (GPU m) where
  return = pure
  x >>= f = GPU $ runGPU x >>= runGPU . f . fst

once :: (Applicative f) => f a -> GPU f a
once = GPU . fmap (id &&& pure)
```

We can then build pure values that will have a side-effect for resource acquisition and then hold
the same value for ever with the `once` function:

```haskell
let buffer = once createBufferIO

-- later, in IO
(_,buffer2) <- runGPU buffer
```

Above, the type of `buffer` and `buffer2` is `GPU IO Buffer`. The first call `runGPU buffer` will
execute the `once` function, calling the `createBufferIO` IO function and will return `buffer2`,
which just stores a pure `Buffer`.

Self-recursion is great to implement local states like that and I advise having a look at the
`Auto` type. You can also read [my article on netwire](http://phaazon.blogspot.fr/2015/03/getting-into-netwire.html), which uses self-recursion a lot.

However, I kinda think that a library should have well defined responsibilities, and building such
a pure interface is not the responsibility of luminance because we can have type-safety and a
stateless API without wrapping everything in that `GPU` type. I think that if we want such a pure
type, we should add it later on, in a 3D engine or a dedicated framework – and that’s actually what
I do for demoscene purposes in another, ultra secret project. ;)

The cool thing with luminance using `MonadIO` is the fact that it’s very easy to use in any kind
of type that developpers want to use in their applications. I really don’t like frameworks which
purpose is clearly not flow control that actually enforce flow control and wrapping types! I don’t
want to end up with a `Luminance` type or `LuminanceApplication` type. It should be simple to use
and seamless.

I actually start to think that I did too much about that pure API design idea. The most important
part of luminance should be type-safety and statelessness. If one wants a pure API, then they
should use FRP frameworks or write their own stuff – with free monads for instance, and it’s
actually funny to build!

The next big steps for luminance will be to clean the uniform interfaces which is a bit inconsistent
and unfriendly to use with render commands. I’ll let you know.
