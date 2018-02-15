# Foreword

## Abstracting what?
[Shaders](http://en.wikipedia.org/wiki/Shader) are very common units in the world of graphics. Even though we’re used to using them for shading[^shading] purposes, they’re not limited to that. Vulgarisation has ripped off the meaning up and down so much that nowadays, a **shader** might have nothing related to shading. If you’re already doing some graphics, you may know *OpenGL* and its *compute shaders*. They have nothing to do with shading.

You might also already know [shader toy](https://www.shadertoy.com/). That’s a great place to host cool and fancy *OpenGL* shaders[^shadertoy_shaders]. You write your shaders in GLSL[^GLSL] then a GLSL compiler is invoked, and your shader is running on the GPU.

## The problem with source based shaders

So you write your shader as a source code in a host language, for instance in *C/C++*, *Java*, *Haskell*, *whatever*, and you end up with a shader running on GPU.

There’re two nasty issues with that way of doing though:

  - the shader is compiled at runtime, so if it contains error, you’ll know that after your application starts ;
  - you have to learn a new language for each target shader compilers.

They’re both serious issues I’m going to explain further.

### Issue n°1: compiled at runtime

This is problematic for a good reason: a lot of shaders are application dependent. **Shadertoy** is a nice exception, just like modeling tools or material editors, but seriously, in most applications, end users are not asked the shaders to run with. In a game for instance, you write all the shaders while writing the game, and then release the whole package.

> *Yeah… What’s about additional content? Per-map shaders, or that kind of stuff?*

Those shaders are like resources. That doesn’t imply using them as is though. We could use dynamic relocatable objects (**.so** or **.dll**) for instance.

> *What compile-time compilation gives you?*

It gives you something hyper cool: **host language features**. If you have a strongly-typed language, you’ll benefit from that. And that’s a huge benefit you can’t get away from. If you’re writing an incorrectly typed shader, your application / library won’t compile, so that the application won’t react in weird way at run-time. That’s pretty badass.

### Issue n°2: languages, languages…

This issue is not as important as the first one, but still. If you’re working on a project and you target several platforms (among ones using *OpenGL*, *OpenGL ES*, *DirectX* and a *soft renderer*), you’ll have to learn several shading languages as well (**GLSL**, **HLSL**[^HLSL]).

In order to solve that, there’re two ways to go:

  - a DSL[^DSL] ;
  - an EDSL[^EDSL].

A DSL is appealing. You have a standalone language for writing shaders, and backends for a compiler/language. However, that sounds a bit overwhelming for such an aim.

An EDSL is pretty cool as well. Take a host language (we’ll be using **Haskell**) and provide structure and construction idioms borrowed from such a language to create a small embedded one. That is the solution I’m going to introduce.

# Ash

**Ash** stands for **Abstract Shader**. It’s a Haskell package I’ve been working on for a few weeks now. The main idea is:

  - to provide a typesafe shading language compiled at compile-time;
  - to provide backends;
  - to provide a nice and friendly *haskellish* interface.

I guessed it’d be a good idea to share my thoughts about the whole concept, since I reckon several people will be interested in such a topic. However, keep in mind that Ash is still a big work in progress. I’m gonna use several blog entries to write down my thoughts, share it with you, possibly enhance Ash, and finally release a decent and powerful library.

If you’re curious, you can find Ash [here](https://github.com/phaazon/ash).

## Basics

**Ash** is a library that provides useful tools to build up *shaders* in **Haskell**. In **Ash**, a *shader* is commonly function. For instance, a vertex shader is a function that *folds* vertex components down to other ones – possibly *maps*, but it could *add*/*remove* components as well – and *yields* extra values for the next stages to work with.

You write a shader with the Ash EDSL then you pass it along to a **backend compiler**.

Here are two examples. In order for you to understand how Ash works, I’ll first write the GLSL (330 core) shader, then the Ash one.

### First example: a simple vertex shader

Let’s write a vertex shader that takes a position and a color, and projects the vertex using a perspective matrix, a view matrix and the object matrix of the object currently being rendered and passes the color to the next stage:

```
#version 330 core

in vec3 pos;
in vec4 col;

out vec4 vcol;

uniform mat4 projViewModel;

void main() {
  vcol = col; 
  gl_Position = projViewModel * vec4(pos, 1.);
}
```

And now, the Ash one:

```
vertexShader :: Ash (M44 Float -> V3 Float :. V4 Float -> V4 Float :. V4 Float)
vertexShader = lam $ \proj -> lam $ \v ->
  let pos :. col = v
  in proj #* v3v4 pos 1 :. col
```

`Ash` is the type used to lift the shading expression up to Haskell. You use it to use the EDSL. It actually represents some kind of HOAST[^HOAST].

Then, you can find `M44`, `V3`, `V4` and `(:.)`.

`M44` is the type of **4x4 matrices**. Since projection matrix, view matrix and model matrix are all 4x4 floating matrix, `M44 Float` makes sense.

`V3` and `V4` represents 3D and 4D vectors, respectively. `V3 Int` is three ints packed in a vector as well as `V4 Float` is four floats packed in a vector. You’ll also meet `V2`, which is… the 2D version.

`(:.)` is a [type operator](https://www.haskell.org/ghc/docs/7.2.1/html/users_guide/data-type-extensions.html#infix-tycons) used to build tuples. You can see `(:.)` as a generalized `(,)` – the default Haskell pair type – but `(:.)` is more power full since it can flatten expressions:

```
a :. (b :. c) = a :. b :. c
```

The `(:.)` has a lot of uses in Ash. In our cases, a chain of `(:.)` represents a vertex’ components.

So our `vertexShader` value is just a function that takes a matrix and a vertex (two components) and outputs two values: the new position of the shader, and the color. Let’s see the body of the function.

```
lam $ \proj -> lam $ \v ->
```

This is a pretty weird expression, but I haven’t found – yet? – a better way to go. `lam` is a combinator used to introduce lambdas in the EDSL. This expression then introduces a lambda that takes two values: `proj` and `v`. You can read that as:

```
\proj v ->
```

Next:

```
let pos :. col = v
```

This is the tricky part. That *let expression* extracts the components out of the vertex and binds them to `pos` and `col` for later use.

```
in proj #* v3v4 pos 1 :. col
```

`(#*)` is a cool operator used to multiply a matrix by a vector, yielding a new vector.

`(v3v4)` is a shortcut used to to build a `V4` using a `V3` by providing the missing value – here, `1`. You’ll find similar functions, like `v2v3` and `v2v4`, to respectively build a `V3` from a `V2` by providing the missing value and build a `V4` from a `V2` by providing the two missing values.

We finally wrap the result in a tuple `(:.)`, and we’re done.

## Features

Ash embeds regular linear expressions (vectors, matrix), textures manipulation, tuples creation, let-bindings, lambda functions (they represent shader stages up to now), and a lot of other features.

Each feature is supposed to have an implementation in a given *backend*. For instance, in the GLSL backend, a lambda function is often turned into the well done `main` function. Its parameters are expanded to as `in` values, and
control parameters are `uniform` variables.

Each backend is supposed to export a `compile` function – the name may varies though. However, each backend is free to compiles to whatever smart they think is. For instance, compiling an Ash shader to GLuint (shader stage) is not very smart since it would use `IO` and handles error a specific way we don’t want it to do. So the GLSL compiler is a function like `glslCompile :: Ash … -> Either CompilationError String`, and the `String` can be used as a regular GLSL source code string you’ll pass to whatever implementation of shader you’ve written.

# What’s next?

I need to finish the implentation of the EDSL, and write the whole GLSL 330 compiler. If it’s a success, I’ll accept pull-requests for other famous compilers (other GLSL version compilers, HLSL, and so on and so forth).

Once that done, I’ll write a few other blog entries with example as a proof-of-concept :)



[^shading]: Shading is the process in which *primitives* (sets of *vertices*) are turned into colors (i.e *fragments*, a.k.a. *pixels* or *texels*).

[^shadertoy_shaders]: Actually, they’re **fragment shaders**.

[^GLSL]: [OpenGL Shading Language](https://www.opengl.org/documentation/glsl/).

[^HLSL]: [High Level Shading Language](http://msdn.microsoft.com/en-us/library/windows/desktop/bb509638(v=vs.85).aspx).

[^DSL]: [Domain-Specific Language](http://en.wikipedia.org/wiki/Domain-specific_language).

[^EDSL]: Embedded Specific Language.

[^HOAST]: [High-Order Abstract Syntax tree](http://en.wikipedia.org/wiki/Higher-order_abstract_syntax); for the purpose of this paper, you don’t have to fully understand them to get your feet wet with Ash (which is cool, right? :) ).
