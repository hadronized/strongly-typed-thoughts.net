# In the previous episode…

This blog entry directly follows the one in which I introduced **Ash**, a *shading language* embedded in **Haskell**. Feel free to [read it here](http://phaazon.blogspot.co.uk/2014/11/foreword-abstracting-what-shaders-are.html) before going on.

# Controlling behavior

A *shader* is commonly a function. However, it’s a bit more than a simple function. If you’re a haskeller, you might already know the `MonadReader` typeclass or simply `Reader` (or its transformer version `ReaderT`). Well, a shader is kind of a function in a reader monad.

> *So… that implies a shader runs in… an environment?*

Yeah, exactly! And you define that environment. The environment is guaranteed not to change between two invocations of a shader for the same render (e.g. between two vertices in the same render). This is interesting, because it enables you to use nice variables, such as time, screen resolution, matrices and whatever your imagination brings on ;)

The environment, however, can be changed between two renders, so that you can update the time value passed to the shader, the new resolution if the window resizes, the updated matrices since your camera’s moved, and so on and so forth.

Let’s see a few example in GLSL first.

## Shader environment in GLSL

To control the environment of a shader in GLSL, we use *uniform* variables. Those are special, global variables and shared between all stages of a shader chain[^shader_chain].

Let’s see how to introduce a few *uniforms* in our shader:

```
uniform float time;       // time of the host application
uniform vec2 resolution;  // (w,h)
uniform vec2 iresolution; // (1 / w, 1 / h), really useful in a lot of cases ;)
uniform mat4 proj;        // projection matrix
uniform int seed;         // a seed for whatever purpose (perlin noise?)
uniform ivec2 gridSize;   // size of a perlin noise grid!
```

You got it. Nothing fancy. Those uniforms are shared between all stages so that we can use `time` in all our shaders, which is pretty cool. You use them as any kind of other variables.

Ok, let’s write an expression that takes a time, a bias value, and multiply them between each other:

```
uniform float time;
uniform float bias;

// this is not a valid shader, just the expression using it
time * bias;
```

## Shader environment in HLSL

**HLSL** uses the term [constant buffers](http://msdn.microsoft.com/en-us/library/windows/desktop/ff476896(v=vs.85).aspx) to introduce the concept of environment. I don’t have any examples right now, sorry for the inconvenience.

## Shader environment in Ash

In Ash, environment variables are not called uniforms nor constant buffers. They’re called… *CPU variables*. That might be weird at first, but let’s think of it. Those values are handled through your application, which lives CPU-side. The environment is like a bridge between the CPU world and the GPU one. A CPU variable refers to a constant value GPU-side, but varying CPU-side.

Create a *CPU variable* is pretty straight-forward. You have to use a function called `cpu`. That function is a monadic function working in the EDSL monad. I won’t describe that type since it’s still a work in progress, but it’s a monad for sure.

> **Note: If you’ve read the [previous blog entry](http://phaazon.blogspot.co.uk/2014/11/foreword-abstracting-what-shaders-are.html), you might have come across the `Ash` type, describing a HOAST. That type is no more a HOAST. The “new Ash” – the type describing the HOAST – is now Expr.**

This is `cpu`:

```
cpu :: (CPU a) => Chain (Expr a)
```

`CPU` is a typeclass that enables a type to be injected in the environment of a shader chain. The instances are provided by Ash and you can’t make your own – do you really want to make `instance CPU String`, or `instance (CPU a) => CPU (Maybe a)`? Don’t think so ;)

Let’s implement the same time–bias example as the GLSL one:

```
foo :: Chain (Expr Float)
foo = liftA2 (*) cpu cpu
```

That example is ridiculous, since in normal code, you’d actually want to pass the CPU variables to nested expressions, in shaders. So you could do that:

```
foo :: Chain ()
foo = do
  time <- cpu
  bias <- cpu

  -- do whatever you want with time and bias
  return ()
```

# You said Chain?

`Chain` is a new type I introduce in this paper. The idea came up from a discussion I had with Edward Kmett when I discovered that the EDSL **needed** a way to bind the CPU variables. I spotted two ways to go:

  - using a name, like `String`, passed to `cpu`; that would result in writing the name in every shader using it, so that’s not ideal;
  - introducing the environment and providing a monad instance so that we could bind the CPU variables and use them in shaders inside the monad.

The latter also provides a nice hidden feature. A chain of shaders might imply varying[^varying] values. Those varying values have information attached. If you mistake them, that results in catastrophic situations. Using a higher monadic type to capture that information – along with the environment – is in my opinion pretty great because it can prevent you from going into the wall ;).

To sum up, `Chain` provides a clear way to describe the relation between shaders.

# What’s next?

I’m still building and enhancing Ash. In the next post, I’ll try to detail the interface to build functions, but I still need to find how to represent them the best possible way.



[^shader_chain]: You can imagine a shader chain as an explicit composition of functions (i.e. shaders). For instance, a vertex shader followed by geometry shader, itself followed by a fragment shader.
[^varying]: Varying values are values that travel between shaders. When a shader outputs a value, it can go to the input of another shader. That is call a varying value.
