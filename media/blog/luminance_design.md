[luminance-0.7.0](https://crates.io/crates/luminance/0.7.0) was released a few days ago and I
decided it was time to explain exactly what luminance is and what were the design choices I made.
After a very interesting talk with [nical](https://github.com/nical) about other rust graphics
frameworks (e.g. [gfx](https://crates.io/crates/gfx), [glium](https://crates.io/crates/glium),
[vulkano](https://crates.io/crates/vulkano), etc.), I thought it was time to give people some more
information about luminance and how to compare it to other frameworks.

# Origin

luminance started as [a Haskell package](https://hackage.haskell.org/package/luminance), extracted
from a *“3D engine”* I had been working on for a while called
[quaazar](https://github.com/phaazon/quaazar). I came to the realization that I wasn’t using the
Haskell garbage collector at all and that I could benefit from using a language without GC. Rust
is a very famous language and well appreciated in the Haskell community, so I decided to jump in and
learn Rust. I migrated luminance in a month or two. The mapping is described in
[this blog entry](http://phaazon.blogspot.fr/2016/04/porting-haskell-graphics-framework-to.html).

# What is luminance for?

I’ve been writing 3D applications for a while and I always was frustrated by how OpenGL is badly
designed. Let’s sum up the lack of design of OpenGL:

- *weakly typed*: OpenGL has types, but… it actually does not. `GLint`, `GLuint` or `GLbitfield` are
  all defined as *aliases* to primary and integral types (i.e. something like
  `typedef float GLfloat`). Try it with `grep -Rn "typedef [a-zA-Z]* GLfloat" /usr/include/GL`. This
  leads to the fact that *framebuffers*, *textures*, *shader stages*, *shader program* or even
  *uniforms*, etc. have the same type (`GLuint`, i.e. `unsigned int`). Thus, a function like
  `glCompileShader` expects a `GLuint` as argument, though you can pass a framebuffer, because it’s
  also represented as a `GLuint` – very bad for us. It’s better to consider that those are just
  untyped – :( – handles.
- *runtime overhead*: Because of the point above, functions cannot assume you’re passing a value of
  a the expected type – e.g. the example just above with `glCompileShader` and a framebuffer. That
  means OpenGL implementations have to check against *all* the values you’re passing as arguments to
  be sure they match the type. That’s basically **several tests for each call** of an OpenGL
  function. If the type doesn’t match, you’re screwed and see the next point.
- *error handling*: This is catastrophic. Because of the runtime overhead, almost all functions
  might set the *error flag*. You have to check the error flag with the `glGetError` function,
  adding a side-effect, preventing parallelism, etc.
- *global state*: OpenGL works on the concept of global mutation. You have a state, wrapped in a
  *context*, and each time you want to do something with the GPU, you have to change something in
  the context. Such a context is important; however, some mutations shouldn’t be required. For
  instance, when you want to change the value of an object or use a texture, OpenGL requires you to
  *bind* the object. If you forget to *bind* for the next object, the mutation will occurs on the
  first object. Side effects, side effects…

The goal of luminance is to fix most of those issues by providing a safe, stateless and elegant
graphics framework. It should be as low-level as possible, but shouldn’t sacrifice
runtime performances – CPU charge as well as memory bandwidth. That is why if you know how to
program with OpenGL, you won’t feel lost when getting your feet wet with luminance.

Because of the many OpenGL versions and other technologies (among them, vulkan), luminance has an
extra aim: abstract over the trending graphics API.

# Types in luminance

In luminance, all graphics resources – and even concepts – have their own respective type. For
instance, instead of `GLuint` for both shader programs and textures, luminance has `Program` and
`Texture`. That ensures you don’t pass values with the wrong types.

> Because of static warranties provided by compile-time, with such a scheme of strong-typing, the
runtime **shouldn’t have to check for type safety**. Unfortunately, because luminance *wraps over*
OpenGL in the luminance-gl backend, we can only add static warranties; we cannot remove the
runtime overhead.

# Error handling

luminance follows the Rust conventions and uses the famous `Option` and `Result` types to specify
errors. You will never have to check against a global error flag, because this is just all wrong.
Keep in mind, you have the `try!` macro in your Rust prelude; use it as often as possible!

> Even though Rust needs to provide an exception handler – i.e. panics – there’s no such thing as
exceptions in Rust. The `try!` macro is just syntactic sugar to:

> ```rust
> match result {
>   Ok(x) => x,
>   Err(e) => return e
> }
> ```

# Stateless

luminance is stateless. That means you don’t have to bind an object to be able to use it. luminance
takes care of that for you in a very simple way. To achieve this and keep performances running, it’s
required to add a bit of high-level to the OpenGL API by leveraging how binds should happen.

Whatever the task you’re trying to reach, whatever computation or problem, it’s always better to
gather / group the computation by batches. A good example of that is how magnetic hard drive disks
work or your RAM. If you spread your data across the disk region (fragmented data) or across several
non-contiguous addresses in your RAM, it will end up by unnecessary moves. The hard drive’s head
will have to go all over the disk to gather the information, and it’s very likely you’ll destroy the
RAM performance (and your CPU caches) if you don’t put the data in a contiguous area.

If you don’t group your OpenGL resources – for instances, you render 400 objects with shader A, 10
objects with shader B, then 20 objects with shader A, 32 objects with shader C, 349 objects with
shader A and finally 439 objects with shader B, you’ll add more OpenGL calls to the equation – hence
more global state mutations, and those are costly.

Instead of this:

1. 400 objects with shader A
2. 10 objects with shader B
3. 20 objects with shader A
4. 32 objects with shader C
5. 348 objects with shader A
6. 439 objects with shader B

luminance **forces** you to group your resources like this:

1. 400 + 20 + 348 objects with shader A
2. 10 + 439 objects with shader B
3. 32 objects with shader C

This is done via types called `Pipeline`, `ShadingCommand` and `RenderCommand`.

## Pipelines

A `Pipeline` gathers shading commands under a `Framebuffer`. That means that all `ShadingCommand`
embedded in the `Pipeline` will output to the embedded `Framebuffer`. Simple, yet powerful, because
we can *bind* the framebuffer when executing the pipeline and don’t have to worry about framebuffer
until the next execution of another `Pipeline`.

## ShadingCommand

A `ShadingCommand` gathers render commands under a shader `Program` along with an update function.
The update function is used to customize the `Program` by providing *uniforms* – i.e. `Uniform`.
If you want to change a `Program`s `Uniform` once a frame – and only if the `Program` is only called
once in the frame – it’s the right place to do it.

All `RenderCommand` embedded in the `ShadingCommand` will be rendered using the embedded shader
`Program`. Like with the `Pipeline`, we don’t have to worry about binding: we just have to use the
embedded shader program when executing the `ShadingCommand`, and we’ll bind another program the next
time a `ShadingCommand` is ran!

## RenderCommand

A `RenderCommand` gathers all the information required to render a `Tessellation`, that is:

- the blending equation, source and destination blending factors
- whether the depth test should be performed
- an update function to update the `Program` being in use – so that each object can have different
  properties used in the shader program
- a reference to the `Tessellation` to render
- the number of instances of the `Tessellation` to render
- the size of the rasterized points (if the `Tessellation` contains any)

# What about shaders?

Shaders are written in… the backend’s expected format. For OpenGL, you’ll have to write **GLSL**.
The backends automatically inserts the version pragma (`#version 330 core` for OpenGL 3.3 for
instance). In the first place, I wanted to migrate **cheddar**, my Haskell shader EDSL. But… the sad
part of the story is that Rust is – yet – unable to handle that kind of stuff correctly. I started
to implement an EDSL for luminance with macros. Even though it was usable, the error handling is
seriously terrible – macros shouldn’t be used for such an important purpose. Then some rustaceans
pointed out I could implement a (rustc) compiler plugin. That enables the use of new constructs
directly into Rust by extending its syntax. This is great.

However, with the hindsight, I will not do that. For a very simple reason. luminance is, currently,
simple, stateless and most of all: it works! I released a PC demo in Köln, Germany using luminance
and a demoscene graphics framework I’m working on:

[pouët.net link](http://www.pouet.net/prod.php?which=67966)

[youtube capture](https://www.youtube.com/watch?v=pYqZS1C_7PU)

[ion demoscene framework](https://github.com/phaazon/ion)

While developping Céleri Rémoulade, I decided to bake the shaders directly into Rust – to get used
to what I had wanted to build, i.e., a shader EDSL. So there’re a bunch of constant `&'static str`
everywhere. Each time I wanted to make a fix to a shader, I had to leave the application, make the
change, recompile, rerun… I’m not sure it’s a good thing. Interactive programming is a very good
thing we can enjoy – yes, even in strongly typed languages ;).

I saw that [gfx](https://crates.io/crates/gfx) doesn’t have its own shader EDSL either and requires
you to provide **several shader implementations (one per backend)**. I don’t know; I think it’s not
that bad if you only target a single backend (i.e. OpenGL 3.3 or Vulkan). Transpiling shaders is a
thing, I’ve been told…

> *sneaking out…*

Feel free to dig in the code of Céleri Rémoulade [here](https://github.com/phaazon/celeri-remoulade).
It’s demoscene code, so it had been rushed on before the release – read: it’s not as clean as I
wanted it to be.

I’ll provide you with more information in the next weeks, but I prefer spending my spare time
writing code than explaining what I’m gonna do – and missing the time to actually do it. ;)

Keep the vibe!
