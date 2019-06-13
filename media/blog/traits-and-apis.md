For those who have been following me for some months now, you might already know that I’ve been
refactoring and redesign from the ground up [luminance]. The `1.0-beta` will arrive as soon as
possible but in the waiting, I thought it could be interesting to share design ideas I had while
redesigning.

I know it’s a thing to do a “This Week In Whatever™”, which is not something I will start because I
already lack spare time to finish all my crates and merge contributions, but I will maybe have my
own “This Month In Luminance” instead.

Today, I wanna talk about a situation I’ve been facing several times, in [luminance] but also in
previous projects (even from other languages!).

# The manual static dispatch burden

There is a concept in [luminance] I’m pretty proud of that ages from its
[Haskell version](https://hackage.haskell.org/package/luminance). It’s the way *shader uniforms* are
handled. Shader uniforms, in [luminance], are write-only variables you can set to customize a pass
of a render in a given shader. Because their nature, they live in only one shader and are not shared
between shader programs (but they’re shared between stages inside a given shader program).

> *“Wait, what are you talking about?! Shader? Stages? Programs?”*

Ah, yes. All the fuzz around raytracing and shaders lately drive people confused. So:

  - *A shader* is a *buzzword*. A graphics programmer will never use the word this way to describe
    something concrete. At last they will use it to describe a rough and abstract concept but not a
    real thing. When you see someone talking about *a shader*, depending on whom, it’s very likely
    they’re actually talking about a *fragment shader*, which is a *shader stage*. Keep on reading.
  - *A shader* stage is a piece of program that runs on a graphics system — I use *graphics system*
    instead of GPU on purpose since you could have shader support without a GPU! As the name
    implies, you can imagine that a graphics system will run several *stages*. Each stage is
    responsible in a given, very specific task. Some common stages we found in demos or games:
    - *A vertex shader* is a *shader stage* that is responsible in transforming vertices that
      flow-in from a vertex stream. When you want to [rasterize] triangles for instance, the
      *vertex shader* will inspect all vertices and possibly move them around, changing their
      coordinates, add them some data, etc. It cannot remove nor add vertices. See it as a `map`
      function.
    - *A geometry shader* does a bit the same job as a *vertex shader*, but instead of working on
      vertices, it works on *primitives* (i.e. lines, triangles, etc.), constructed from the output
      of the previously active *vertex shader*. So a *geometry shader* comes next after a *vertex
      shader*. Geometry shaders can remove vertices or add ones, change data, etc. See them as a
      `concat_map` function: for each primitive (triangle would be 3 vertices), they can generate
      no vertex data, or as many as the input, or add more. Geometry shaders are VERY useful when
      you want to enrich geometry on the fly or do some tricky transformation.
    - *A fragment shader* will run as part of one of the last part of the rasterizer and will be
      run on all covered pixels by your mutilated triangle from the previously active *geometry
      shader*. The output of *fragment shaders* are color channels, mostly.
  - *A shader program* is a pipeline comprised of several stages. Some of them are optional (you
    don’t have to use a geometry shader or tessellation shaders, for instance) and some are
    mandatory (you need a vertex shader and a fragment shader).

That was a big digression but now you might know something new about graphics programming! In the
rest of document, I will always talk about stages or programs and never about *shaders*.

I will not explain the rationale between how [luminance] does to provide a clean and safe API above
uniforms (this is the topic for a whole blog post). Instead, I’m going to give you the problem of
the day.

Uniforms are abstracted by the `Uniform<T>` type in [luminance]. `T` represents the type of the
variable on the graphics system. For instance, if you want to change 4D 32-bit floating value, you
will do it via a `Uniform<[f32; 4]>`. So far, so good.

The problem is that the OpenGL API is a bit boring with uniforms. Each type has a specific function
to call. For instance, to send 4 `f32` as uniforms, you must use the [`glUniform4f`] function. If
you want to pass a single 32-bit integer (`i32`), you will use `glUniform1i`. Etc., etc.

Maybe you already see the problem. We need a way to restrict the user on the set of types they can
use… and for each type, we need to call a specific function. We could restrict the types via several
methods:

  - Algebraic types (enums with data): this would allow us to declare all the supported types at
    once. however, we will then have to branch at run time to perform the dispatch, which is not
    something I want.
  - Use a trait

[luminance]: https://crates.io/crates/luminance
[rasterize]: https://en.wikipedia.org/wiki/Rasterisation
[`glUniform4f`]: https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glUniform.xhtml
