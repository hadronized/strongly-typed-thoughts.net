I’ve been up working on vertex arrays in my work-in-progress graphics
framework, [luminance](https://github.com/phaazon/luminance), for several
days. I’m a bit slow, because I’ve been through a very hard breakup and have
been struggling to recover and focus. But here I am!

# So, what’s new?

**OpenGL** allows programmers to send *vertices* to the GPU through what
is called a [vertex array](https://www.opengl.org/wiki/Vertex_Specification).
Vertex specification is performed through several functions, operating on
several objects. You need, for instance, a *vertex buffer object*, an *index
buffer object* and a *vertex array object*. The *vertex buffer* stores the
vertices data.

![Teapot](http://wiki.splashdamage.com/upload/2/2f/Teapot_mesh.jpg)

For instance, you could imagine a *teapot* as a set of vertices. Those
vertices have several attributes. We could use, for instance, a **position**, a
**normal** and a **bone index**. The vertex buffer would be responsible of storing those
positions, normals and bone indices. There’re two ways to store them:

  1. interleaved arrays ;
  2. deinterleaved arrays.

I’ll explain those later on. The *index buffer* stores integral numbers –
mainly set to `unsigned int` – that index the vertices, so that we can connect
them and create lines, triangles or more complex shapes.

Finally, the *vertex array object* is a state object that stores links to the
two buffers and makes a connection between pointers in the buffer and attribute
indices. Once everything is set up, we might only use the *vertex array object*.
The exception is when we need to change the geometry of an object. We need to
access the vertex buffer and the index buffer and upload new data. However,
**for now**, that feature is disabled so that the buffers are not exposed to the
programmer. If people think that feature should be implemented, I’ll create
specialized code for that very purpose.

## Interleaved and deinterleaved arrays

Interleaved arrays might be the most simple to picture, because you use such
arrays every day when programming. Let’s imagine you have the following type in
**Haskell**:

```haskell
data Vertex = Vertex {
    vertPos    :: X
  , vertNor    :: Y
  , vertBoneID :: Z
  } deriving (Eq,Show)
```

Now, the teapot would have several vertices. Approximately, let’s state the
teapot has five vertices – yeah, ugly teapot. We can represent such vertices
in an interleaved array by simply recording them in a list or an array:

![Interleaved](http://phaazon.net/pub/interleaved.png)

As you can see, the attributes are interleaved in memory, and the whole pattern
is cycling. That’s the common way to represent an array of struct in a lot of
languages, and it’s very natural for a machine to do things like that.

The deinterleaved version is:

![Deinterleaved](http://phaazon.net/pub/deinterleaved.png)

As you can see, with deinterleaved arrays, all attributes are extracted and
grouped. If you want to access the third vertex, you need to read the third `X`,
the third `Y` and the third `Z`.

Both the methods have advantages and drawbacks. The cool thing about
deinterleaved arrays is that we can copy huge regions of typed memory at once
whilst we cannot with interleaved arrays. However, interleaved arrays store
continuous structures, so writing and reading a structure back might be faster.

An important point to keep in mind: because we plan to pass those arrays to
**OpenGL**, there’s no
[alignment](https://en.wikipedia.org/wiki/Data_structure_alignment) restriction
on the structure. That is, everything is *packed*, and we’ll have to pass extra
information to **OpenGL** to tell it how to advance in memory to correctly
build vertices back.

## Generalized tuple

I think I haven’t told you yet. I have a cool type in
[luminance](https://github.com/phaazon/luminance): the `(:.)` type. No, you
don’t have to know how to pronounce that. I like to call it the *gtuple*
type, because it’s a generalized tuple. You can encode `(a,b)`, `(a,b,c)` and
all kind of tuples with `(:.)`. You can even encode single-typed infinite
tuple! – a very special kind of list, indeed.

```haskell
data a :. b = a :. b

infixr 6 :.

-- a :. b is isomorphic to (a,b)
-- a :. b :. c is isomorphic to (a,b,c)

newtype Fix f = Fix (f (Fix f)) -- from Control.Monad.Fix
type Inf a = Fix ((:.) a) -- infinite tuple!
```

Pretty simple, but way more powerful than the regular, monomorphic tuples. As
you can see, `(:.)` is a right-associative. That means that
`a :. b :. c = a :. (b :. c)`.

That type will be heavily used in
[luminance](https://github.com/phaazon/luminance), thus you should get your fet
wet with it. There’s actually nothing much to know about it. It’s a `Functor`.
I might add other features to it later on.

### The Storable trick

The cool thing about `(:.)` is that we can provide a `Storable` instance for
packed memory, as **OpenGL** requires it. Currently, the `Storable` instance
is [implemented like this](https://github.com/phaazon/luminance/blob/05ef2e4879aae92189535f0121765931b20de2fd/src/Graphics/Luminance/Tuple.hs#L23):

```haskell
instance (Storable a,Storable b) => Storable (a :. b) where
  sizeOf (a :. b) = sizeOf a + sizeOf b
  alignment _ = 1 -- packed data
  peek p = do
    a <- peek $ castPtr p
    b <- peek . castPtr $ p `plusPtr` sizeOf (undefined :: a)
    pure $ a :. b
  poke p (a :. b) = do
    poke (castPtr p) a
    poke (castPtr $ p `plusPtr` sizeOf (undefined :: a)) b
```

As you can see, the `alignment` is set to `1` to express the fact the memory
is packed. The `peek` and `poke` functions use the size of the head of the tuple
to advance the pointer so that we effectively write the whole tuple in packed
memory.

Then, let’s rewrite our `Vertex` type in terms of `(:.)` to see how it’s going
on:

```haskell
type Vertex = X :. Y :. Z
```

If `X`, `Y` and `Z` are in `Storable`, we can directly `poke` one of our
`Vertex` into a [luminance buffer](http://phaazon.blogspot.fr/2015/07/introducing-luminance-safer-opengl-api.html)! That is, directly into the GPU buffer!

Keep in mind that the `Storable` instance implements packed-memory uploads and
reads, and won’t work with special kinds of buffers, like *shader storage* ones,
which require specific memory alignment. To cover them, I’ll create specific
typeclasses instances. No worries.

# Creating a vertex array

Creating a vertex array is done through the function `createVertexArray`. I
might change the name of that object – it’s ugly, right? Maybe `Shape`, or
something cooler!

```haskell
createVertexArray :: (Foldable f,MonadIO m,MonadResource m,Storable v,Traversable t,Vertex v)
                  => t v
                  -> f Word32
                  -> m VertexArray
```

As you can see, the type signature is highly polymorphic. `t` and `f` represent
*foldable* structures storing the vertices and the indices. And that’s all.
Nothing else to feed the function with! As you can see, there’s a typeclass
constraint on `v`, the inner vertex type, `Vertex`. That constraint ensures the
vertex type is representable on the **OpenGL** side and has a known vertex
format.

**Disclaimer:** the `Traversable` constraint might be relaxed to be `Foldable`
very soon.

Once tested, I’ll move all that code from the `unstable` branch to the `master`
branch so that you guys can test it. :)

## About OpenGL…

I eventually came to the realization that I needed to inform you about the
**OpenGL** prerequisites. Because I want the framework to be as modern and
well-designed as possible, you’ll need… **OpenGL 4.5**. The latest version,
indeed. You **might** also need an extension, [ARB_bindless_texture](https://www.opengl.org/wiki/Bindless_Texture). That would enable the framework to pass textures to
shader in a very stateless way, which is our objective!

I’ll let you know what I decide about that. I don’t want to use an extension
that is not implemented almost everywhere.


# What’s next?

Well, tests! I need to be sure everything is correctly done on the GPU side,
especially the vertex format specification. I’m pretty confident though.

Once the vertex arrays are tested, I’ll start defining a *render interface* as
stateless as I can. As always, I’ll keep you informed!

