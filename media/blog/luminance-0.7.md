# luminance-0.7

You can’t even imagine how hard it was to release [luminance-0.7](http://hackage.haskell.org/package/luminance-0.7).
I came accross several difficulties I had to spend a lot of time on but finally, here it is. I made
a lot of changes for that very special release, and I have a lot to say about it!

## Overview

As for all my projects, I always provide people with a *changelog*. The 0.7 release is a major 
release (read as: it was a major increment). I think it’s good to tell people what’s new, but it
should be **mandatory** to warn them about what has changed so that they can directly jump to their
code and spot the uses of the deprecated / changed interface.

Anyway, you’ll find patch, minor and major changes in luminance-0.7. I’ll describe them in order.

## Patch changes

### Internal architecture and debugging

A lot of code was reviewed internally. You don’t have to worry about that. However, there’s a new
cool thing that was added internally. It could have been marked as a minor change but it’s not
*supposed* to be used by common people – you can use it via a flag if you use `cabal` or `stack`
though. It’s about debugging the *OpenGL* part used in luminance. You shouldn’t have to use it but
it could be interesting if you spot a bug someday. Anyway, you can enable it with the flag
`debug-gl`.

### Uniform Block / Uniform Buffer Objects

The [UBO](https://www.opengl.org/wiki/Uniform_Buffer_Object) system was buggy and was fixed. You
might experience issue with them though. I spotted a bug and reported it – you can find the bug
report [here](https://bugs.freedesktop.org/show_bug.cgi?id=92909). That bug is not Haskell related
and is related to the i915 Intel driver.

## Minor changes

The minor changes were the most important part of luminance-0.7. luminance now officially supports
*OpenGL 3.2*! When installing luminance, you default to the `gl32` backend. You can select the
backend you want with flags – `gl45` and `gl45-bindless-textures` – but keep in mind you need the
appropriate hardware to be able to use it. Because you need to use flags, you won’t be able to
switch to the backend you want at runtime – that’s not the purpose of such a change though.

The performance gap should be minor between `gl32` and `gl45` but still. Basically, OpenGL 4.5 adds
the support for [DSA](https://www.opengl.org/wiki/Direct_State_Access), which is very handy and less
ill-designed that previous iterations of OpenGL. So a lot of code had to be rewritten to implement
luminance’s stateless interface without breaking performance nor bring them down.

I *might* add support for other backends later on – like an *OpenGL ES* backend and *WebGL* one –
but that won’t ship that soon though because I have a ton of work to do, and yet need to provide you
with a concrete, beautiful, fast, appealing and eye-blowing demo with luminance! ;)

Feel free to test the `gl32` backend and give me back feedback!

However, if I spent so much time on that 0.7 version, it’s because I had issue whilst writing the
`gl32` backend. Indeed, I spotted several bugs on my Intel HD card. This is my OpenGL version string
for my Intel IGP card:

> OpenGL core profile version string: 3.3 (Core Profile) Mesa 11.0.4

The architecture is Haswell. And on such a card (`i915` linux driver) I’ve found two bugs while
trying the `gl32` backend with [luminance-samples-0.7](https://hackage.haskell.org/package/luminance-samples-0.7).

### `usampler2D`

For unknown reason, the `Texture` sample failed on my Intel IGP but ran smoothly and flawlessly on
my nVidia GPU. I spent a lot of time trying to figure out what I was missing, but eventually changed
the sampler type – it’s now a `sampler2D` – and… it worked. I reported the issue to the intel dev
team. So if you hit that error too, please leave a message here so that I can have more hindsight
about that error and see what I can do.

### Uniform block and `vec3`

This is a very nasty issue that kept me awoken for days trying to fix **my** code while it was a
driver bug. It’s a big technical, so I’ll just leave a [link to the bug tracker](https://bugs.freedesktop.org/show_bug.cgi?id=92909)
so that you can read it if you want to.

## Breaking changes

Ok, let’s talk.

When creating a new shader stage, you now have to use the function `createStage` – instead of
several functions like `createVertexShader`. That change is important so that I can add new shader
types without changing the interface, and because some shader can fail to be created. For instance,
on the `gl32` backend, trying to build a tessellation shader will raise an error.

When a shader stage creation fails, the `UnsupportedStage` error is raised and holds the type of the
stage that failed.

Finally, the interface for the cubemaps changed a bit – you don’t have access to *width* and
*height* anymore, that was error-prone and useless; you’re stick to a *size* parameter.

I’d like to thank all people supporting me and luminance. I’ll be watching reactions to that major
and important release as it will cover more people with cheaper and well-spread GPUs.

Happy hacking! :)
