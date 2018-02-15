A few weeks ago, I was writing **Haskell** lines for a project I had been
working on for a very long time. That project was a 3D engine. There are several
posts about it on my blog, feel free to check out.

The thing is… Times change. The more it passes, the more I become mature in what
I do in the **Haskell** community. I’m a demoscener, and I need to be
productive. Writing a whole 3D engine for such a purpose is a good thing, but I
was going round and round in circles, changing the whole architecture every now
and then. I couldn’t make my mind and help it. So I decided to stop working on
that, and move on.

If you are a **Haskell** developer, you might already know
[Edward Kmett](https://www.fpcomplete.com/user/edwardk?show=all). Each talk with
him is always interesting and I always end up with new ideas and new knowledge.
Sometimes, we talk about graphics, and sometimes, he tells me that writing a
3D engine from scratch and release it to the community is not a very good move.

I’ve been thinking about that, and in the end, I agree with Edward. There’re two
reasons making such a project hard and not interesting for a community:

  1. a good “3D engine” is a specialized one – for FPS games, for simulations,
     for sport games, for animation, etc. If we know what the player will do, we
     can optimize a lot of stuff, and put less details into not-important part
     of the visuals. For instance, some games don’t really care about skies, so
     they can use simple skyboxes with nice textures to bring a nice touch of
     atmosphere, without destroying performance. In a game like a flight
     simulator, skyboxes have to be avoided to go with other techniques to provide
     a correct experience to players. Even though an engine could provide both
     techniques, apply that problem to almost everything – i.e. space partitionning
     for instance – and you end up with a nightmare to code ;
  2. an engine can be a very bloated piece of software – because of point *1*.
     It’s very hard to keep an engine up to date regarding technologies, and
     make every one happy, especially if the engine targets a large audience of
     people – i.e. [hackage](http://hackage.haskell.org).

Point *2* might be strange to you, but that’s often the case. Building a
flexible 3D engine is a very hard and non-trivial task. Because of point *1*,
you utterly need to restrict things in order to get the required level of
performance or design. There are people out there – especially in the demoscene
world – who can build up 3D engines quickly. But keep in mind those engines are
limited to demoscene applications, and enhancing them
to support something else is not a trivial task. In the end, you might end up
with a lot of bloated code you’ll eventually zap later on to build something
different for another purpose – eh, demoscene is about going dirty, right?! ;)

# Basics

So… Let’s go back to the basics. In order to include everyone, we need to
provide something that everyone can download, install, learn and use. Something
like [OpenGL](https://www.opengl.org/sdk). For **Haskell**, I highly recommend
using [gl](https://hackage.haskell.org/package/gl). It’s built against the
`gl.xml` file – released by Khronos. If you need sound, you can use the
complementary library I wrote, using the same name convention,
[al](https://hackage.haskell.org/package/al).

The problem with that is the fact that **OpenGL** is a low-level API. Especially
for new comers or people who need to get things done quickly. The part that
bothers – wait, no, *annoys* – me the most is the fact that **OpenGL** is a very
old library which was designed two decades ago. And we suffer from that. A lot.

**OpenGL** is a *stateful* graphics library. That means it maintains a *state*, a
*context*, in order to work properly. Maintaining a context or state is a legit
need, don’t get it twisted. However, if the design of the API doesn’t fit such a
way of dealing with the state, we come accross a lot of problems. Is there *one*
programmer who hasn’t experienced black screens yet? I don’t think so.

The **OpenGL**’s API exposes a lot of functions that perform *side-effects*.
Because **OpenGL** is weakly typed – almost all objects you can create in
**OpenGL** share the same `GL(u)int` type, which is very wrong – you might end
up doing nasty things. Worse, it uses an internal binding system to *select* the
objects you want to operate on. For instance, if you want to upload data to a
texture object, you need to *bind* the texture before calling the texture upload
function. If you don’t, well, that’s bad for you. There’s no way to verify code
safety at compile-time.

You’re not convinced yet? **OpenGL** doesn’t tell you directly how to change
things on the GPU side. For instance, do you think you have to *bind* your vertex
buffer before performing a render, or is it sufficient to *bind* the *vertex array
object* only? All those questions don’t have direct answers, and you’ll need to
dig in several wikis and forums to get your answers – the answer to that question
is *“Just bind the VAO, pal.”*

# What can we do about it?

Several attempts to enhance that safety have come up. The first thing we
**have** to do is to wrap all **OpenGL** object types into proper types. For
instance, we need several types for `Texture` and `Framebuffer`.

Then, we need a way to ensure that we cannot call a function if the context is
not setup for. There are a few ways to do that. For instance,
[indexed monads](https://hackage.haskell.org/package/indexed-0.1) can be a good
start. However, I tried that, and I can tell you it’s way too complicated. You
end up with very long types that make things barely unreadable.
[See this](https://github.com/phaazon/igl/blob/master/src/Graphics/Rendering/IGL/Buffer.hs#L114)
and [this](https://github.com/phaazon/igl/blob/master/src/Graphics/Rendering/IGL/GL.hs#L51)
for excerpts.

## Luminance

In my desperate quest of providing a safer **OpenGL**’s API, I decided to create
a library from scratch called [luminance](https://github.com/phaazon/luminance).
That library is not really an **OpenGL** safe wrapper, but it’s very close to
being that.

`luminance` provides the same objects than **OpenGL** does, but via a safer way
to create, access and use them. It’s an effort for providing safe abstractions
without destroying performance down and suited for graphics applications. It’s
not a 3D engine. It’s a rendering framework. There’s no *light*, *asset
managers* or that kind of features. It’s just a *tiny* and *simple* powerful
API.

### Example

`luminance` is still a huge *work in progress*. However, I can already show an
example. The following example opens a window  but doesn’t render anything.
Instead, it creates a buffer on the GPU and perform several simple operations
onto it.

```haskell
-- Several imports.
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Resource -- from the resourcet package
import Data.Foldable ( traverse_ )
import Graphics.Luminance.Buffer
import Graphics.Luminance.RW
import Graphics.UI.GLFW -- from the GLFW-b package
import Prelude hiding ( init ) -- clash with GLFW-b’s init function

windowW,windowH :: Int
windowW = 800
windowH = 600

windowTitle :: String
windowTitle = "Test"

main :: IO ()
main = do
  init
  -- Initiate the OpenGL context with GLFW.
  windowHint (WindowHint'Resizable False)
  windowHint (WindowHint'ContextVersionMajor 3)
  windowHint (WindowHint'ContextVersionMinor 3)
  windowHint (WindowHint'OpenGLForwardCompat False)
  windowHint (WindowHint'OpenGLProfile OpenGLProfile'Core)
  window <- createWindow windowW windowH windowTitle Nothing Nothing
  makeContextCurrent window
  -- Run our application, which needs a (MonadIO m,MonadResource m) => m
  -- we traverse_ so that we just terminate if we’ve failed to create the
  -- window.
  traverse_ (runResourceT . app) window
  terminate

-- GPU regions. For this example, we’ll just create two regions. One of floats
-- and the other of ints. We’re using read/write (RW) regions so that we can
-- send values to the GPU and read them back.
data MyRegions = MyRegions {
    floats :: Region RW Float
  , ints   :: Region RW Int
  }

-- Our logic.
app :: (MonadIO m,MonadResource m) => Window -> m ()
app window = do
  -- We create a new buffer on the GPU, getting back regions of typed data
  -- inside of it. For that purpose, we provide a monadic type used to build
  -- regions through the 'newRegion' function.
  region <- createBuffer $
    MyRegions
      <$> newRegion 10
      <*> newRegion 5
  clear (floats region) pi -- clear the floats region with pi
  clear (ints region) 10 -- clear the ints region with 10
  readWhole (floats region) >>= liftIO . print -- print the floats as an array
  readWhole (ints region) >>= liftIO . print -- print the ints as an array
  floats region `writeAt` 7 $ 42 -- write 42 at index=7 in the floats region
  floats region @? 7 >>= traverse_ (liftIO . print) -- safe getter (Maybe)
  floats region @! 7 >>= liftIO . print -- unsafe getter
  readWhole (floats region) >>= liftIO . print -- print the floats as an array
```

Those read/write regions could also have been made *read-only* or *write-only*.
For such regions, some functions can’t be called, and trying to do so will make
your compiler angry and throw errors at you.

Up to now, the buffers are created *persistently* and *coherently*. That might
cause issues with **OpenGL** *synchronization*, but I’ll wait for benchmarks
before changing that part. If benchmarking spots performance bottlenecks, I’ll
introduce more buffers and regions to deal with special cases.

`luminance` doesn’t force you to use a specific windowing library. You can then
embed it into any kind of host libraries.

# What’s to come?

[luminance](https://github.com/phaazon/luminance) is very young. At the moment
of writing this article, it’s only 26 commits old. I just wanted to present it
so that people know it exists will be released as soon as possible. The idea is
to provide a library that, if you use it, won’t create black screens because of
framebuffers incorrectness or buffers issues. It’ll ease debugging **OpenGL**
applications and prevent from making nasty mistakes.

I’ll keep posting about `luminance` as I get new features implemented.

As always, keep the vibe, and happy hacking!
