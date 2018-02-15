It’s been a few days I haven’t talked about
[luminance](http://hackage.haskell.org/package/luminance). I’ve been working on it a lot those days
along with [wavefront](http://hackage.haskell.org/package/wavefront). In order that you keep up to
date, I’ll describe the changes I made in those packages you have a talk about the future directions
of those packages.

I’ll also give a snippet you can use to load geometries with wavefront and adapt them to embed into
luminance so that you can actually render them! A package might come up from that kind of snippet –
`luminance-wavefront`? We’ll see that!

# wavefront

This package has received several changes among two major increments and several fixes. In the first
place, I removed some code from the interface that was useless and used only for test purposes. I
removed the `Ctxt` object – it’s a type used by the internal lexer anyways, so you don’t have to
know about it – and exposed a type called [WavefrontOBJ](http://hackage.haskell.org/package/wavefront-0.4.0.1/docs/Codec-Wavefront.html#t:WavefrontOBJ).
That type reprents the parsed Wavefront data and is the *main* type used by the library in the
interface.

Then, I also removed most of the modules, because they’re re-exported by the main module –
[Codec.Wavefront](http://hackage.haskell.org/package/wavefront-0.4.0.1/docs/Codec-Wavefront.html).
I think the documentation is pretty straight-forward, but you think something is missing, please
shoot a PM or an email! ;)

On the bugs level, I fixed a few things. Among them, there was a nasty bug in the implementation of
an internal recursive parser that caused the last wavefront statement to be silently ignored.

I’d also like to point out that I performed some benchmarks – I will provide the data later on with
a heap profile and graphs – and I’m pretty astonished with the results! The parser/lexer is insanely
fast! It only takes a few milliseconds (between 7ms and 8ms) to load 50k faces (a 2MB .obj file).
The code is not yet optimized, so I guess the package could go even faster!

You can find the changelog [here](http://hackage.haskell.org/package/wavefront-0.4.0.1/changelog).

# luminance

I made a *lot of work* on luminance lately. First, the `V` type – used to represent *vertex
components* – is not anymore defined by luminance but by [linear](http://hackage.haskell.org/package/linear).
You can find the type [here](http://hackage.haskell.org/package/linear-1.20.2/docs/Linear-V.html).
You’ll need the `DataKinds` extension to write types like `V 3 Float`.

That change is due to the fact linear is a mature library with a lot of interesting functions and
types everyone might use when doing graphics. Its [`V`](http://hackage.haskell.org/package/linear-1.20.2/docs/Linear-V.html)
type has several interesting instances – `Storable`, `Ord`, etc. – that are required in luminance.
Because it’s not simple to build such `V`, luminance provides you with three functions to build the
1D, 2D and 3D versions – [`vec2`](http://hackage.haskell.org/package/luminance-0.5.1/docs/Graphics-Luminance-Vertex.html#v:vec2),
[`vec3`](http://hackage.haskell.org/package/luminance-0.5.1/docs/Graphics-Luminance-Vertex.html#v:vec3)
and [`vec4`](http://hackage.haskell.org/package/luminance-0.5.1/docs/Graphics-Luminance-Vertex.html#v:vec2).
Currently, that type is the only one you can use to build *vertex components*. I might add `V2`,
`V3` and `V4` as well later.

An interesting change: the `Uniform` typeclass has **a lot** of new instances! Basically, all vector
types from linear, their array version and the 4x4 floating matrix – `M44 Float`. You can find the
list of all instances [here](http://hackage.haskell.org/package/luminance-0.5.1/docs/Graphics-Luminance-Shader-Uniform.html#t:Uniform).

A new function was added to the [`Graphics.Lumimance.Geometry`](http://hackage.haskell.org/package/luminance-0.5.1/docs/Graphics-Luminance-Geometry.html)
module called [`nubDirect`](http://hackage.haskell.org/package/luminance-0.5.1/docs/Graphics-Luminance-Geometry.html#v:nubDirect).
That function performs in linear logarithmic time and is used to turn a *direct* representation of
vertices into a pair of data used to represent *indexed* vertices. The new list of vertices
stores only unique vertices and the list of integral values stores the indices. You can then use
both the information to build *indexed geometries* – see
[`createGeometry`](http://hackage.haskell.org/package/luminance-0.5.1/docs/Graphics-Luminance-Geometry.html#v:createGeometry)
for further details.

The interface to transfer texels to textures has changed. It doesn’t depend on `Foldable` anymore
but on `Data.Vector.Storable.Vector`. That change is due to the fact that the `Foldable` solution
uses `toList` behind the hood, which causes bad performance for the simple reason that we send the
list to the GPU through the FFI. It’s then more efficient to use a `Storable` version. Furthermore,
th  most known package for textures loading – [JuicyPixels](http://hackage.haskell.org/package/JuicyPixels)
– already uses that type of `Vector`. So you just have to enjoy the new performance boost! ;)

About bugs… I fixed a few ones. First, the implementation of the `Storable` instance for
[`(:.)`](http://hackage.haskell.org/package/luminance-0.5.1/docs/Graphics-Luminance-Core-Tuple.html#t::.)
had an error for `sizeOf`. The implementation must be lazy in its argument, and the old one was not,
causing `undefined` crashes when using that type. The strictness was removed and now everything
works just fine!

Two bugs that were also fixed: the indexed render and the render of geometries with several vertex
components. Those bugs were easy to fix and now you won’t experience those issues anymore.

# Interfacing luminance with wavefront to render geometries from artists!

I thought it would be a hard task but I’m pretty proud of how easy it was to interface both the
packages! The idea was to provide a function that would turn a `WavefrontOBJ` into a *direct
representation* of luminance vertices. Here’s the function that implements such a conversion:

```haskell
type Vtx = V 3 Float :. V 3 Float -- location :. normal

objToDirect :: WavefrontOBJ -> Maybe [Vtx]
objToDirect obj = traverse faceToVtx (toList faces)
  where
    locations = objLocations obj
    normals = objNormals obj
    faces = objFaces obj
    faceToVtx face = do
      let face' = elValue face
      vni <- faceNorIndex face'
      v <- locations !? (faceLocIndex face' - 1)
      vn <- normals !? (vni - 1)
      let loc = vec3 (locX v) (locY v) (locZ v)
          nor = vec3 (norX vn) (norY vn) (norZ vn)
      pure (loc :. nor)
```

As you can see, that function is pure and will eventually turn a `WavefrontOBJ` into a list of
`Vtx`. `Vtx` is our own vertex type, encoding the location and the normal of the vertex. You can
add texture coordinates if you want to. The function fails if a face’s index has no normal
associated with or if an index is out-of-bound.

And… and that’s all! You can already have your `Geometry` with that – *direct* one:

```haskell
  x <- fmap (fmap objToDirect) (fromFile "./ubercool-mesh.obj")
  case x of
    Right (Just vertices) -> createGeometry vertices Nothing Triangle
    _ -> throwError {- whatever you need as error there -}
```

You want an indexed version? Well, you already have everything to do that:

```haskell
  x <- fmap (fmap (nubDirect . objToDirect) (fromFile "./ubercool-mesh.obj")
  case x of
    Right (Just (vertices,indices)) -> createGeometry vertices (Just indices) Triangle
    _ -> throwError {- whatever you need as error there -}
```

Even though the `nubDirect` performs in a pretty good complexity, it takes time. Don’t be surprised
to see the “loading” time longer then.

I might package those snippets and helpers around them into a `luminance-wavefront` package, but
that’s not trivial as the vertex format should be free.

# Future directions and thank you

I received a lot of warm feedback from people about what I do in the Haskell community, and I’m just
amazed. I’d like to thank each and everyone of you for your support – I even got support from
non-Haskellers!

What’s next then… Well, I need to add a few more textures to luminance – texture arrays are not
supported yet, and the framebuffers have to be altered to support all kind of textures. I will also
try to write a [cheddar]() interpreter directly into luminance to dump the `String` type of shader
stages and replace it with cheddar’s whatever will be. For the long terms, I’ll add UBO and SSBO to
luminance, and… compatibility with older OpenGL versions.

Once again, thank you, and keep the vibe!
