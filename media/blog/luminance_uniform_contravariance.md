It’s been a few days I haven’t posted about [luminance](https://github.com/phaazon/luminance). I’m
on holidays, thus I can’t be as involved in the development of the graphics framework as I’m used to
on a daily basis. Although I’ve been producing less in the past few days, I’ve been actively
thinking about something very important: [uniform](https://www.opengl.org/wiki/Uniform_%28GLSL%29).

# What people usually do

Uniforms are a way to pass data to shaders. I won’t talk about *uniform blocks* nor *uniform
buffers* – I’ll make a dedicated post for that purpose. The common OpenGL uniform flow is the
following:

  1. you ask OpenGL to retrieve the location of a GLSL uniform through the function
     `glGetUniformLocation`, or you can use an explicit location if you want to handle the semantics
     on your own ;
  2. you use that location, the identifier of your shader program and send the actual values with
     the proper `glProgramUniform`.

You typically don’t retrieve the location each time you need to send values to the GPU – you only
retrieve them once, while initializing.

The first thing to make uniforms more elegant and safer is to provide a typeclass to provide a
shared interface. Instead of using several functions for each type of uniform – `glProgramUniform1i`
for `Int32`, `glProgramUniform1f` for `Float` and so on – we can just provide a function that will
call the right OpenGL function for the type:

```haskell
class Uniform a where
  sendUniform :: GLuint -> GLint -> a -> IO ()

instance Uniform Int32 where
  sendUniform = glProgramUniform1i

instance Uniform Float where
  sendUniform = glProgramUniform1f

-- and so on…
```

That’s the first step, and I think everyone should do that. However, that way of doing has several
drawbacks:

  - it still relies on side-effects; that is, we can call `sendUniform` pretty much everywhere ;
  - imagine we have a shader program that **requires** several uniforms to be passed each time we
    draw something; what happens if we forget to call a `sendUniform`? If we haven’t sent the
    uniform yet, we might have an undefined behavior. If we already have, we will *override* all
    future draws with that value, which is very wrong… ;
  - with that way of representing uniforms, we have a very imperative interface; we can have a more
    composable and pure approach than that, hence enabling us to gain in power and flexibility.

# What luminance used to do

In my [luminance](https://github.com/phaazon/luminance) package, I used to represent uniforms as
values.

```haskell
newtype U a = U { runU :: a -> IO () }
```

We can then alter the `Uniform` typeclass to make it simpler:

```haskell
class Uniform a where
  toU :: GLuint -> GLint -> U a

instance Uniform Int32 where
  toU prog l = U $ glProgramUniform1i prog l

instance Uniform Float where
  toU prog l = U $ glProgramUniform1f prog l
```

We also have a pure interface now. I used to provide another type, `Uniformed`, to be able to
*send* uniforms without exposing `IO`, and an operator to accumulate uniforms settings, `(@=)`:

```haskell
newtype Uniformed a = Uniformed { runUniformed :: IO a } deriving (Applicative,Functor,Monad)

(@=) :: U a -> a -> Uniformed ()
U f @= a = Uniformed $ f a
```

Pretty simple.

# The new uniform interface

The problem with that is that we still have the completion problem and the side-effects, because we
just wrap them without adding anything special – `Uniformed` is isomorphic to `IO`. We have no way
to create a type and ensure that *all* uniforms have been sent down to the GPU…

## Contravariance to save us!

If you’re an advanced **Haskell** programmer, you might have noticed something very interesting
about our `U` type. It’s contravariant in its argument. What’s cool about that is that we could then
create new uniform types – new `U` – by contramapping over those types! That means we can enrich
the scope of the hardcoded `Uniform` instances, because the single way we have to get a `U` is
to use `Uniform.toU`. With contravariance, we can – in theory – extend those types to **all types**.

Sounds handy eh? First thing first, contravariant functor. A contravariant functor is a functor that
flips the direction of the morphism:

```haskell
class Contravariant f where
  contramap :: (a -> b) -> f b -> f a
  (>$) :: b -> f b -> f a
```

`contramap` is the *contravariant* version of `fmap` and `(>$)` is the contravariant version of
`(<$)`. If you’re not used to contravariance or if it’s the first time you see such a type
signature, it might seem confusing or even **magic**. Well, that’s the mathematic magic in the
place! But you’ll see just below that there’s no magic no trick in the implementation.

Because `U` is contravariant in its argument, we can define a `Contravariant` instance:

```haskell
instance Contravariant U where
  contramap f u = U $ runU u . f
```

As you can see, nothing tricky here. We just apply the `(a -> b)` function on the input of the
resulting `U a` so that we can pass it to `u`, and we just `runU` the whole thing.

A few friends of mine – not **Haskeller** though – told me things like *“That’s just theory
bullshit, no one needs to know what a contravariant thingy stuff is!”*. Well, here’s an example:

```haskell
newtype Color = Color {
    colorName :: String
  , colorValue :: (Float,Float,Float,Float)
  }
```

Even though we have an instance of `Uniform` for `(Float,Float,Float,Float)`, there will never be an
instance of `Uniform` for `Color`, so we can’t have a `U Color`… Or can we?

```haskell
uColor = contramap colorValue float4U
```

The type of `uColor` is… `U Color`! That works because contravariance enabled us to *adapt* the
`Color` structure so that we end up on `(Float,Float,Float,Float)`. The contravariance property is
then a very great ally in such situations!

## More contravariance

We can even dig in deeper! Something cool would be to do the same thing, but for several fields.
Imagine a mouse:

```haskell
data Mouse = Mouse {
    mouseX :: Float
  , mouseY :: Float
  }
```

We’d like to find a cool way to have `U Mouse`, so that we can send the mouse cursor to shaders.
We’d like to contramap over `mouseX` and `mouseY`. A bit like with `Functor` + `Applicative`:

```haskell
getMouseX :: IO Float
getMouseY :: IO Float

getMouse :: IO Mouse
getMouse = Mouse <$> getMouseX <*> getMouseY
```

We could have the same thing for contravariance… And guess what. That exists, and that’s called
**divisible contravariant functors**! A `Divisible` contravariant functor is the exact contravariant
version of `Applicative`!

```haskell
class (Contravariant f) => Divisible f where
  divide :: (a -> (b,c)) -> f b -> f c -> f a
  conquer :: f a
```

`divide` is the contravariant version of `(<*>)` and `conquer` is the contravariant version of
`pure`. You know that `pure`’s type is `a -> f a`, which is isomorphic to `(() -> a) -> f a`. Take
the contravariant version of `(() -> a) -> f a`, you end up with `(a -> ()) -> f a`. `(a -> ())` is
isomorphic to `()`, so we can simplify the whole thing to `f a`. Here you have `conquer`. *Thank you
to Edward Kmett for helping me understand that!*

Let’s see how we can implement `Divisible` for `U`!

```haskell
instance Divisible U where
  divide f p q = U $ \a -> do
    let (b,c) = f a
    runU p b
    runU q c
  conquer = U . const $ pure ()
```

And now let’s use it to get a `U Mouse`!

```haskell
let uMouse = divide (\(Mouse mx my) -> (mx,my)) mouseXU mouseYU
```

And here we have `uMouse :: U Mouse`! As you can see, if you have several uniforms – for each fields
of the type, you can `divide` your type and map all fields to the uniforms by applying several times
`divide`.

The current implementation is almost the one shown here. There’s also a `Decidable` instance, but
I won’t talk about that for now.

The cool thing about that is that I can lose the `Uniformed` monadic type and rely only on `U`.
Thanks to the `Divisible` typeclass, we have completion, and we can’t override future uniforms then!

----------------

I hope you’ve learnt something cool and useful through this. Keep in mind that category abstractions 
**are powerful** and are useful in some contexts.

Keep hacking around, keep being curious. A **Haskeller** never stops learning! And that’s what so
cool about **Haskell**! Keep the vibe, and see you another luminance post soon!
