Up to now, [luminance](https://hackage.haskell.org/package/luminance) has been lacking two cool
features: [UBO](https://www.opengl.org/wiki/Uniform_Buffer_Object) and
[SSBO](https://www.opengl.org/wiki/Shader_Storage_Buffer_Object). Both are *buffer-backed* uniform
techniques. That is, a way to pass uniforms to shader stages through buffers.

The [latest version of luminance](https://hackage.haskell.org/package/luminance-0.6) has one of the
two features. *UBO* were added and *SSBO* will follow for the next version, I guess.

# What is UBO?

UBO stands for **U**niform **B**buffer **O**bject. Basically, it enables you to create *uniform
blocks* in GLSL in feed them with *buffers*. Instead of passing values directly to the uniform
interface, you just write whatever values you want to to buffers, and then pass the buffer as a
source for the uniform block.

Such a technique has a lot of advantages. Among them, you can pass **a lot of values**. It’s also
cool when you want to pass values instances of a structure (in the GLSL source code). You can also
use them to share uniforms between several shader programs as well as quickly change all the
uniforms to use.

In luminance, you need several things. First thing first, you need… a buffer! More specifically,
you need a buffer [`Region`](https://hackage.haskell.org/package/luminance-0.6/docs/Graphics-Luminance-Buffer.html#t:Region)
to store values in. However, you cannot use any kind of region. You have to use a region that can
hold values that will be fetched from shaders. This is done with a type called `UB a`. A buffer of
`UB a` can be used as *UBO*.

Let’s say you want to store colors in a buffer, so that you can use them in your fragment shader.
We’ll want three colors to shade a triangle. We need to create the buffer and get the region:

```haskell
colorBuffer :: Region RW (UB (V3 Float)) <- createBuffer (newRegion 3)
```

The explicit type is there so that GHC can infer the correct types for the `Region`. As you can see,
nothing fancy, except that we just don’t want a `Region RW (V3 Float` but
`Region RW (UB (V3 Float))`. Why `RW`?

Then, we’ll want to store colors in the buffer. Easy peasy:

```haskell
writeWhole colorBuffer (map UB colors)

colors :: [V3 Float]
colors = [V3 1 0 0,V3 0 1 0,V3 0 0 1] -- red, green, blue
```

At this point, `colorBuffer` represents a GPU buffer that holds three colors: red, green and blue.
The next part is to get the uniform interface. That part is experimental in terms of exposed
interface, but the core idea will remain the same. You’re given a function to build *UBO* uniforms
as you also have a function to build simple and plain uniforms in
[`createProgram`](https://hackage.haskell.org/package/luminance-0.6/docs/Graphics-Luminance-Shader-Program.html#v:createProgram):

```haskell
createProgram shaderList $ \uni uniBlock -> {- … -}
```

Don’t spend too much time reading the signature of that function. You just have to know that
`uni` is a function that takes `Either String Natural` – either a uniform’s name or its integral
semantic – and gives you mapped `U` in return and that `uniBlock` does the same thing, but for
uniform blocks instead.

Here’s our vertex shader:

```C
in vec2 co;
out vec4 vertexColor;

// This is the uniform block, called "Colors" and storing three colors
// as an array of three vec3 (RGB).
uniform Colors {
  vec3 colors[3];
};

void main() {
  gl_Position = vec4(co, 0., 1.);
  vertexColor = vec4(colors[gl_VertexID], 1.);
}"
```

So we want to get a `U a` mapped to that `"Colors"` uniform block. Easy!

```haskell
(program,colorsU) <- createProgram shaderStages $ \_ uniBlock -> uniBlock "Colors"
```

And that’s all! The type of `colorsU` is `U (Region rw (UB (V3 Float)))`. You can then gather 
`colorBuffer` and `colorsU` in a uniform interface to send `colorBuffer` to `colorsU`!

You can find the complete sample [here](https://github.com/phaazon/luminance-samples/blob/c4f428dc68ed38f0b80032aa9de90df01bf8ab15/src/UBO.hs).

Finally, you can augment the type you can use `UB` with by implementing the `UniformBlock`
typeclass. You can derive the `Generic` typeclass and then use a default instance:

```haskell
data MyType = {- … -} deriving (Generic)

instance UniformBlock MyTpe -- we’re good to go with buffer of MyType!
```

# luminance, luminance-samples and Stackage

I added *luminance* and *luminance-samples* into Stackage. You can then find them in the nightly
snapshots and the future LTS ones.

# What’s next?

I plan to add stencil support for the framebuffer, because it’s missing and people might like it
included. I will of course add support for *SSBO** as soon as I can. I also need to work on cheddar
but that project is complex and I’m still stuck with design decisions.

Thanks for reading my and for your feedback. Have you great week!
