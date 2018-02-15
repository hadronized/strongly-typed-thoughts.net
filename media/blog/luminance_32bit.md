Yesterday, I released a new article about how I implement vertex arrays in
luminance. In that article, I told you that the memory was packed with
alignment set to **1**.

Well, I’ve changed my mind. Some people pointed out that the good thing to do
for most GPU is to align on 32-bit. That is, **4** bytes. The alignment should
be **4** bytes, then, not **1**.

There might be an issue with that. If you store a structure with attributes
which sizes are not a multiple of **4** bytes, it’s likely you need to add
padding.

However, I just reviewed my code, and found this:

```haskell
instance (GPU a,KnownNat n,Storable a) => Vertex (V n a) where
instance (Vertex a,Vertex b) => Vertex (a :. b) where
```

Those are the single instances for `Vertex`. That means you can only use `V`
and `(:.)` to build up vertices. Look at the `V` instance. You’ll find a `GPU`
typeclass constraint. Let’s look at its definition and instances:

```haskell
class GPU a where
  glType :: Proxy a -> GLenum

instance GPU Float where
  glType _ = GL_FLOAT

instance GPU Int32 where
  glType _ = GL_INT

instance GPU Word32 where
  glType _ = GL_UNSIGNED_INT

```
Woah. How did I forget that?! Let me translate those information to you. That
means we can only have 32-bit vertex component! So the memory inside vertex
buffers will always be aligned on **4** bytes! No need to worry about padding
then!

The first implication is the fact you won’t be able to use `Word16`, for
instance. You’ll need to stick to the three types that have a `GPU` instance.

**Note**: that doesn’t prevent us from adding `Double` later on, because a
`Double` is a 64-bit type, which is a multiple of **4** bytes!

That’s all I have for today. I’m working on something very exciting linked to
render batching. I’ll talk about that when it’s cooked. ;)

Keep the vibe; keep building awesome things, and as always, thank you for
reading me!
