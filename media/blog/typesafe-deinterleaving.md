[luminance-0.40] is finally out, after several months of hardwork. A blog post is available
[here][0] if you’ve missed it.

Among all the new features of [luminance-0.40], there is one that I think is worth explaining in
details, because it’s… _pretty cool_. If you like _type states_, _refinement typing_ and on a
general level, strong typing, you should enjoy this blog article.

While working on [luminance-0.40], I spent two weeks working on a feature that is going to have a
huge beneficial impact on how people use luminance. I’ve been wanting to see what people think about
it for a long time, because I think it’s both a very powerful feature and allows to do low-level
memory stuff in a safe way via the type system. Without further ado, let’s dig in.

<!-- vim-markdown-toc GFM -->

* [The Tess type](#the-tess-type)
  * [Digression on interleaved and deinterleaved data structures](#digression-on-interleaved-and-deinterleaved-data-structures)
  * [The Tess type… revisited](#the-tess-type-revisited)
* [The magic of type systems](#the-magic-of-type-systems)
* [Deinterleaving compile-time dispatching](#deinterleaving-compile-time-dispatching)
* [Wrap it up](#wrap-it-up)

<!-- vim-markdown-toc -->

# The Tess type

In pre **luminance-0.40**, it is possible to gather _vertices_ inside a type called `Tess` (it
stands for _tessellation_). That type is responsible for holding vertices to render them later.
They might represent point clouds, triangle-based meshes, lines, etc. It contains several properties,
but we are going to focus on three:

- The actual _vertex_ data (i.e. the held vertices).
- _Indices_, that can be used to prevent duplicating vertices and index them to reduce the size
  of the mesh.
- _Vertex instances_, which are special data that are instance-dependent. In a graphics library or
  a graphics engine, an _instance_ often refers to an instance of a template, a pattern, that is to
  be shared among instances: the instances simply add some specific values, like positions, colors,
  etc. Imagine in a simplistic video game a garden with trees: it’s likely that the trees (all of
  them) are rendered via a single draw call, which contains instance data that is going to
  customize each instance.

In **luminance-0.39**, vertices, indices and instances can be retrieved, both read-only on
read-write, via a mechanism of GPU slicing: you ask luminance to give you a type with which you are
able to use `&[T]` and `&mut [T]`.

However… we have several problems. First, the type is `Tess` and is completely monomorphized. What
it means is that if you create a `Tess` that contains vertices of type `SimpleVertex` with indices
of type `u8`, that information is not encoded in the type system — it is actually encoded as a
value that is read and checked at runtime! When you ask for a slice, for instance to mutate the
content of the `Tess`, you have to use a function like [`Tess::as_slice<V> where V: Vertex`][1], which
expects you to pass the type of stored vertices — in our case, it would be `SimpleVertex`. What
happens if someone passed the wrong type? Well, currently, **luminance** checks at runtime that the
stored type is the right one, but this is both wasted checks and not a very elegant API.

The same applies to indices and instance data: you don’t see them in the type. What happens if
you slice the indices with `u32`? Runtime checks.

Now, there’s also the problem of building. When you build a `Tess`, you have to pass the vertices
to the `TessBuilder` type, using functions like [`TessBuilder::add_vertices`][2]. It works, but
it’s not practical. More importantly: if you call several times that function, you will create
something that is called a _deinterleaved memory_ data structure. Let’s digress a bit about that.

## Digression on interleaved and deinterleaved data structures

Interleaving and deinterleaving makes sense when we talk about several objects / items of a same
type, laid out in memory — typically in arrays. Imagine a type, `Vertex`, such as:

```rust
struct Vertex {
  position: Point3D,
  color: RGBA,
}
```

If you take a vector of `Vertex` (`Vec<Vertex>`), you get a continuous array of properties in
memory. If you represent that in memory, you have something similar to this for two vertices
(padding omitted):

```
[x0, y0, z0, r0, g0, b0, a0, x1, y1, z1, r1, g1, b1, a1]
```

We say that the memory is _interleaved_, because you’re going to alternate between fields when
iterating in memory. Everything is interleaved. This kind of memory is what happens when you put a
`struct` in an array, slice, etc. and is perfectly suited for most situations (even on the GPU).
However, there is a small (yet important) detail: when you iterate (for instance in a `for` loop)
on that array, you’re going to ask your CPU / GPU to load a bunch of values at once. That is going
to fill your _cache lines_. If at each iteration you need to use every fields of the vertex, then
that situation is pretty convenient, because you’re going to have a bunch of fields cached ahead
(cache hits).

However… imagine a loop that only needs to access the `position` field. What’s going to happen is
that your CPU / GPU will still load the same data in cache lines: now you get colors in the cache
that you don’t need and your loop will make more cache misses. What could have been better would
have been to fill the cache lines only with positions. If we had, instead:

```
[x0, y0, z0, x1, y1, z1]
[r0, g0, b0, a0, r1, g1, b1, a1]
```

Those two vectors can then be used independently for each need. Because we only need positions, we
can simply use the first position vector. Now, when the CPU / GPU is going to load something in the
cache, it’s going to cache much more values that we are going to actually use: we get more cache
hits and it’s _playa party_.

That kind of memory layout is called _deinterleaved memory_. The way we typically do that is by
simply moving the fields out of the `struct` and make several arrays of each field.

People tend to use two terms to describe both layouts: [AoS and SoA][3].

So… ~~months~~ _years_ ago, I realized that and decided a needed I better plan. Especially, on
**luminance-0.39**, the way you handle slicing _deinterleaved data_ is… well, inexistent. You
cannot slice such data because it was never supported.

## The Tess type… revisited

The new type is the following:

```rust
pub struct Tess<B, V, I = (), W = (), S = Interleaved>
where ABunchOfThings;
```

As you can see, there are a lot of new things there:

- `B`: the new _backend type_ variable that most types now have; used to forward backend
  implementations down the API.
- `V`: the _vertex type_. It’s the type of `Vertex` you have typically defined yourself using
  [luminance-derive].
- `I`: the _index type_. Most of the time, you’ll use either `u8`, `u16` or `u32`, depending on
  the kind of geometry you index.
- `W`: the _vertex instance data_. Because people might not want that, `()` is a good default, but
  if you really need it, you can basically use any `Vertex` type here, since vertex instance data
  must be part of a `Semantics`.
- `S`: the interleaving mode. Either `Interleaved` or `Deinterleaved`.

You will find the same type variables with the `TessBuilder` type.

# The magic of type systems

The cool thing about that change is how it enabled me to yield much, _much_ better APIs. Consider
the [previous API to create a deinterleaved `Tess`](https://github.com/phaazon/luminance-rs/blob/luminance-0.39/luminance-examples/src/hello-world.rs#L214-L219):

```rust
let direct_deinterleaved_triangles = TessBuilder::new(&mut surface)
  .add_vertices(TRI_DEINT_POS_VERTICES)
  .add_vertices(TRI_DEINT_COLOR_VERTICES)
  .set_mode(Mode::Triangle)
  .build()
  .unwrap();
```

Notice the two `add_vertices`. There is no type information checking and ensuring that:

- You are passing vertex data that are compatible with the `Vertex` type those fields correspond
  to.
- You can basically call `add_vertices` as many times as you want and get a big buffer in GPU
  memory that will make no sense.

Now, the new API [looks like this](https://github.com/phaazon/luminance-rs/blob/luminance-0.40/luminance-examples/src/hello-world.rs#L182-L188):

```rust
let direct_deinterleaved_triangles = surface
  .new_deinterleaved_tess::<Vertex, ()>()
  .set_attributes(&TRI_DEINT_POS_VERTICES[..])
  .set_attributes(&TRI_DEINT_COLOR_VERTICES[..])
  .set_mode(Mode::Triangle)
  .build()
  .unwrap();
```

If you try to call `set_vertices` — the name got changed from `add_vertices` to `set_vertices`
— on the builder you get from `new_deinterleaved_tess`, you will get a compilation error, because you
cannot set vertices on deinterleaved tessellations: you need to set attribute vectors. The
`set_attributes` has the information that you are doing that for a `Vertex`, so it can check the
input data you pass and ensure it contains values which type is a field type used in `Vertex`. If
not, you get a compilation error.

Most importantly: because of how vertices work in luminance, a field type is unique to a vertex: it
doesn’t make sense to use twice the `VertexPosition` type. If you end up in such a situation, it
means that your `Semantics` type lacks another variant — remember: vertex fields are basically
semantics-based attributes. That leads to the possibility to automatically find out where exactly
the data you provide needs to go inside the GPU tessellation.

The super cool part is that you can now slice deinterleaved tessellations by simply asking for:

- The slice of vertices.
- _A_ slice of attributes.
- The slice of indices.

In our case, we have a deinterleaved tessellation, which means we cannot slice whole vertices. If
you try to get a slice of `Vertex`, you will get a compilation error. However, we can retrieve slices
of vertex fields. The way we do this is super simple: we simply call the `tess.vertices()` or
`tess.vertices_mut()` methods. It will infer the type of slices you are asking to automatically
slice the right GPU buffer. This is all possible because our types are  unique as the vertex fields.

```rust
let positions = tess.vertices().unwrap(); // you have to check for errors normally
let colors = tess.vertices().unwrap(); // you have to check for errors normally
```

> Remark: you need to have the types of `positions` and `colors` inferred by setting / reading /
> passing them around, or you will have to put type ascriptions so that `vertices()` know what
> fields you are referring to.

# Deinterleaving compile-time dispatching

So let’s dig a bit into how all this works. The first thing you need to know is that
deinterleaving — the raw concept — is really simple. If you have a type such as:

```rust
struct Foo {
  a: A,
  b: B,
  c: C,
}
```

We say that `Vec<Foo>` is the interleaved representation of the collection. The deinterleaved
representation needs to have three vectors of fields:

```rust
vec_a: Vec<A>
vec_b: Vec<B>
vec_c: Vec<C>
```

In order for a `Vertex` type to be valid in deinterleaving contexts, we need to have that
tuple of vectors representation. First, we need a mapping between `Vec<Foo>` to
`(Vec<A>, Vec<B>, Vec<C>)`. This is the role of two traits: `Deinterleave<T>` and
`TessVertexData<S>`.

`Deinterleave<T>` gives, for `T`, the field rank for `T`. For instance:

- `<Foo as Deinterleave<A>>::RANK == 0`
- `<Foo as Deinterleave<B>>::RANK == 1`
- `<Foo as Deinterleave<C>>::RANK == 2`

This is mandatory so that we know exactly which GPU buffers will need to be read / written to
when creating tessellations and slicing them. You don’t have to implement `Deinterleave<T>` by
yourself: [luminance-derive] does that automatically for you when you create a `Vertex` type.
Also, you might be tempted to think that this rank will be used inside the `Vertex` type to
retrieve data, but since you cannot pass whole vertices… nah. Also, you shouldn’t assume ranks
based on fields declarations in struct (rustc can re-order that).

Next is `TessVertexData<S>`. It associates an input type — the type of data a tessellation will
receive at creation type — for `S`, for the implementor type. The easy one
is `TessVertexData<Interleaved> for V where V: Vertex`. The associated type is simply `Vec<V>`,
because interleaved geometry simply stores the vertices as a vector of the whole vertex type.
Simple.

It gets more complicated when we talk about deinterleaved geometry.
`TessVertexData<Deinterleaved> for V where V: Vertex` has its associated type set to…
`Vec<DeinterleavedData>`. Indeed: there is no simple way with the current stable Rust (and even
nightly) to know the full type of `Vertex` fields at compile-time here. However, don’t get it wrong:
that `Vec` is not a vector of vertices. It’s a typically small set of attributes (vectors  too). If
you look at the definition of `DeinterleavedData`, you get this:

```rust
#[derive(Debug, Clone)]
pub struct DeinterleavedData {
  raw: Vec<u8>,
  len: usize,
}
```

Yep. Type erasure at its finest. When you pass deinterleaved data, the data is type-erased and
passed as a big blob of bytes, glued with its original size (so that we don’t have to store typing
information – this will be needed when slicing vertex attributes).

Implementing slicing with these traits and data types is now possible: we can add another trait
that we will use to slice vertices, for instance (the `backend::VertexSlice` trait in luminance)
and based on the type of `T` in `Deinterleave<T>`, we can go and grab the GPU buffer we want.
For instance, in both the OpenGL and WebGL luminance backends, buffers are stored in a `Vec<_>`,
so in order to know which one we need to lookup, we simple use the
`<Vertex as Deinterleave<T>>::RANK` constant value (a `usize`) and we’re good to go. You
need two other traits for vertex instance data (`InstanceSlice`) and vertex indices (`IndexSlice`),
and you’re good to go.

# Wrap it up

So, checking at compile-time deinterleaving, in luminance, is done by:

- Using a trait (`Deinterleave<T>`) to associate a _rank_ to a field `T` in a data structure.
- Using a trait (`TessVertexData<S>`) to get the input type of a tessellation: either `Vec<T>` for
  the whole vertices, or `Vec<DeinterleavedData>` for a set of attributes).
- When asking for a typed slice, such as `vertices_mut::<T>()`, we basically simply require
  `Deinterleave<T> for V` so that we can get a `RANK: usize`. We can then, in the backend
  implementation, find the right GPU buffer and slice it. The reconstruction to a typed slice
  (`&[T]`) is statically assured by the length stored in the `DeinterleavedData` type and by the
  fact `DeinterleavedData<T>` is implemented for `V`.

A small note though. luminance — actually, I do that with all my code, whatever the language —
considers a lot of operations `unsafe`. In this case, implementing `Deinterleave<T>` is `unsafe`.
The reason is pretty obvious: you can implement `Deinterleave<Whatever>`, even if your vertex
type doesn’t have a `Whatever` field. Doing so would yield a hazardous / undefined behavior when
slicing the GPU buffer. [luminance-derive] takes care of implementing the `unsafe` interface for
you.

I hope you have learned something new or gotten some ideas. Keep the vibes!

[luminance-0.40]: https://crates.io/crates/luminance/0.40.0
[luminance-derive]: https://crates.io/crates/luminance-derive
[0]: https://phaazon.net/blog/luminance-0.40
[1]: https://docs.rs/luminance/0.39.0/luminance/tess/struct.Tess.html#method.as_slice
[2]: https://docs.rs/luminance/0.39.0/luminance/tess/struct.TessBuilder.html#method.add_vertices
[3]: https://en.wikipedia.org/wiki/AoS_and_SoA
