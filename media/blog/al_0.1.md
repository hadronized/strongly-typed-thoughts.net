# What is OpenAL?

![](http://upload.wikimedia.org/wikipedia/en/thumb/2/28/OpenAL_logo.png/200px-OpenAL_logo.png)

[OpenAL](http://www.openal.org/) is an open-source and free audio library
developped by **Creative Technology**. The first version, however, was made by
**Loki Software**. The API targets spacialized audio and sound managing so that
it’s simple to simulate sound attenuation over distance and
[Doppler effect](http://en.wikipedia.org/wiki/Doppler_effect). **OpenAL** is
supported on a [wide variety of platforms](http://en.wikipedia.org/wiki/OpenAL#Supported_platforms),
from PC (Windows, Linux, FreeBSD, Mac OSX and so on) to consoles (PS3, Xbox…).

The latest version was released in 2005 (*9 years old ago!*) and is OpenAL 1.1.
It’s then pretty logical that we should have OpenAL 1.1 in **Haskell**.

# OpenAL and Haskell

> *“Wait… we already have [OpenAL](https://hackage.haskell.org/package/OpenAL)
in Haskell!”*

That’s true. However, the [OpenAL](https://hackage.haskell.org/package/OpenAL)
Haskell package has some serious issues.

The first one is that package is not a direct binding to **OpenAL**. It’s a
high-level wrapper that wraps stuff with obscure types, like `StateVar` or
`ObjectName`. Even though such abstractions are welcomed, we should have a raw
binding so that people who don’t want those abstractions can still use
**OpenAL**. Moreover, such high libraries tend to lift resources allocations /
deallocations into the GC[^GC]. That is a pretty bad idea because we,
sometimes, want to control how long objects live.

The second one, which is as terrible, is the fact that those abstractions aren’t
defined by the Haskell [OpenAL](https://hackage.haskell.org/package/OpenAL)
package, but by another, which is…
[OpenGL](https://hackage.haskell.org/package/OpenGL). The latter is **not** a
raw binding to **OpenGL** – for that have a look at
[OpenGLRaw](https://hackage.haskell.org/package/OpenGLRaw) or
[gl](https://hackage.haskell.org/package/gl) – but the same kind of high-level
oriented package. A lot of us have asked the authors of both the package to
agree of making those abstractions shared between both the libraries; they
never seemed to agree.

And even though, having a dependency between **OpenAL** and **OpenGL** is
insane!

Sooooo… since [Edward Kmett](https://github.com/ekmett) had the idea of
[gl](https://hackage.haskell.org/package/gl) to fix similar issues, I think I
should have the idea of [al](https://hackage.haskell.org/package/al) to go along
with [gl](https://hackage.haskell.org/package/gl). The idea is that if you want
to use one, you can use the other one without any conflict.

# al 0.1

Yesterday night, around 20:00, I decided to make it. It took me several hours
to write the raw binding, but I finally made it and released
[al 0.1](https://hackage.haskell.org/package/al) the very next day – uh, today!

Because **OpenAL** is not shipped with your [^OS] nor anything special, and thus
because you have to explicitely install it, installing
[al](https://hackage.haskell.org/package/al) requires a bit more than just
typing `cabal install al`.

## Installing al

### cabal update

First things first:

    cabal update

You should have [al](https://hackage.haskell.org/package/al) available if
you type `cabal info al` afterwards.

### c2hs

[al](https://hackage.haskell.org/package/al) requires you to have
[c2hs](https://hackage.haskell.org/package/c2hs-0.23.1) – version **0.23.\*** –
installed. If not:

    cabal install c2hs

Be sure it’s correctly installed:

    c2hs --version
    C->Haskell Compiler, version 0.23.1 Snowbounder, 31 Oct 2014
      build platform is "i386-mingw32" <1, True, True, 1>

### OpenAL SDK

Of course, you should have the **OpenAL SDK** installed as well. If not, install
it. It should be in your package manager if you’re on Linux / FreeBSD, and you
can find it [here](http://www.openal.org/creative-installers/) for Windows
systems.

> **Important: write down the installation path, because we’ll need it!**

### Installing al

Here we are. In order to install, [al](https://hackage.haskell.org/package/al)
needs the paths to your **OpenAL SDK**. I’m not sure whether it could vary a
lot, though. The default Windows paths are:

  - `C:\Program Files (x86)\OpenAL 1.1 SDK\libs` for the libraries ;
  - `C:\Program Files (x86)\OpenAL 1.1 SDK\include` for the header files.

In future releases, I’ll default to those so that you don’t have to explicitely
pass the paths if you have a default installation. But for the moment, you have
to pass those paths when installing:

    cabal install al --extra-lib-dirs="C:\Program Files (x86)\OpenAL 1.1 SDK\libs"
    --extra-include-dirs="C:\Program Files (x86)\OpenAL 1.1 SDK\include"

Of course, you may adapt paths to your situation. You may even try without that
for UNIX systems – it might already be in your `PATH`, or use slashs for
**MinGW**.

## Using al

[al](https://hackage.haskell.org/package/al) lays into two module trees:

  - `Sound.AL`
  - `Sound.ALC`

The former exports anything you might need to use the **OpenAL API** while
the latter exports symbols about the **OpenAL Context API**. Please feel free
to dig in each tree to import specific types or functions.

## OpenAL documentation

[al](https://hackage.haskell.org/package/al) doesn’t have any documentation for
a very simple reason: since it’s a raw binding to the **C OpenAL API**, you
should read the **C** documentation. Translating to **Haskell** is
straightforward.

[The OpenAL 1.1 Specifications](http://www.openal.org/documentation/openal-1.1-specification.pdf)

# What’s next?

Up to now, [al](https://hackage.haskell.org/package/al) doesn’t cover
extensions. It’s not shipped with **ALUT** either as I want to keep the package
low-level and raw – and don’t even try to make me change my mind about that ;) .

If you have any issues, please let me know on the issues tracker. I’m responsive
and I’ll happily try to help you :) .

Once again, have fun, don’t eat too much chocolate nor sweets, and happy Easter
Day!

[^GC]: Garbage Collector
[^OS]: Operating System
