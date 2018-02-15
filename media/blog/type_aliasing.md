# Type aliasing

Type aliasing is something that’s been included in most languages for a while
now. Basically, if you have a type `Foo`, aliasing `Bar` to `Foo` means that
you introduce a new *symbol* `Bar` that refers to `Foo`. It’s not a new type,
it’s just an alias, a *binding*.

In **Haskell**, we do aliasing with the keyword `type`, which is a bit stupid
since we won’t necessarily alias a type (we could alias a function, a typeclass
constraint, and so on and so forth).

Aliasing is great and useful in many ways. For instance, there’s a Haskell
package I love called [linear](https://hackage.haskell.org/package/linear). That
package is used to deal with linear algebra, and you can find common math types
in there among vectors (`Vec`) and low-algebra vectors (`V2`, `V3`, `V4`). There
are the same things for matrices (`M22` for *2x2* matrices, `M33`, `M44`).

If you come up with using *4x4* floating matrices a lot, you might end up with a
lot of `M44 Float` everywhere in your code. So that would be sane aliasing it.
Like so:

```haskell
type V4F = V4 Float
```

This is a perfect aliasing use to me as `V4F` is straight-forward for
*graphics* people. Furthermore, `linear` is pretty a standard now in Haskell
linear algebra application, so `V4` should be familiar to you.

However, there is a drawback with type aliasing.

# Why has type aliasing almost killed me

I spent more than a day crawling for a bug. I was refactoring code, a lot of
code. Nothing fancy though, just some code cleaning. Nevertheless, I created a
brand new git branch to make my cleaning. On the initial branch, the application
behavior was normal. After having cleaned the code, I got a black screen.

After a long investigation, I found out that the bug was due to a mismatch
between two matrices – i.e. `M44 Float`. One matrix is used as a camera
projection and the other one is used as a light projection. In term of code,
both matrices are placed in a type. I simplified (a lot), but this is the idea:

```haskell
data Foo = Foo {
    -- […]
  , cameraProjection :: M44 Float
  , lightProjection :: M44 Float
  }
```

What makes those matrices different in term of compilation? Nothing.
`M44 Float` and `M44 Float` are the same type. Swapping matrices is then
silently compiled, since it’s not an error to the compiler. And such a swapping
result in a terrible and hard to find bug.

> *Yeah, so we could have simply aliased those matrices to two different names!*

Remember what I said earlier. Aliased types are not new types. They’re bindings
in the type system:

```haskell
type CameraProjection = M44 Float
type LightProjection = M44 Float
```

Here, both `CameraProjection` and `LightProjection` are the same type. If a
function expects a `CameraProjection`, it actually expects a `M44 Float`. You
could then pass a value of type `M44 Float` or even `LightProjection`, that
would be legit.

> *Is there a solution yet?*

Yes, there is. **Haskell** has several keywords to create types:

  - `type`, which is, as we’ve already seen, a way to alias types;
  - `data`, which is used to create data types;
  - `newtype`, which is used as a type wrapper to introduce a data type.

`newtype` is the most important keyword for us here. Basically, it has the same
semantic as `data` except that `newtype` introduces a type with a single
*field*; it can’t have none nor several. `newtype` is often used to express the
*same thing* that the inner type, but adding / removing some properties.

For instance, `Bool` can’t be an instance of `Monoid` since we could define a
lot of instances. `mempty` could be `True` if we consider the monoid of
booleans with **AND** but it would be `False` if we consider the monoid of
booleans with **OR**. Well, we could write those, actually:

```haskell
newtype And = And { getAnd :: Bool } deriving WhateverYouWant
newtype Or = Or { getOr :: Bool } deriving WhateverYouWant

instance Monoid And where
  mempty = True
  And a `mappend` And b = And (a && b)

instance Monoid Or where
  mempty = False
  Or a `mappend` Or b = Or (a || b)
```

Those two types are defined in `Data.Monoid` as `All` and `Any`.

Back to our problem. We could use `newtype` to add `M44 Float` the correct
semantic, and disable us to mix them:

```haskell
newtype CameraProjection = CameraProjection { getCameraProjection :: M44 Float }
newtype LightProjection = LightProjection { getLightProjection :: M44 Float }
```

And here, `CameraProjection` ≠ `LightProjection`. If a function expects a
`CameraProjection`, you can’t pass anything else than a `CameraProjection`.

However, be careful. You’ll have to wrap a `M44 Float` into your
`CameraProjection`. Such a function, like the `CameraProjection` constructor, is
not semantic safe since you could still pass a light representation.
Nevertheless, it’s better than the initial design since once wrapped, you can’t
make confusions anymore.

# Final words

To sum up, use aliasing when it’s handy but don’t use it if it could break
semantics or add confusion. If you have several types that use the same
type (e.g. colors and positions could use the same `V3 Float` type), **don’t
use aliases**! Create proper types to prevent confusion later on. Your compiler
will reward you, then you don’t get nasty runtime bugs ;).
