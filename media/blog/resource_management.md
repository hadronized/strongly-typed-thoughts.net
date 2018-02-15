# Foreword

In a real time rendering system, it’s not uncommon finding constructs about
*assets*. One famous construct is the *resource manager*. A *resource manager* is
responsible of several tasks, among:

  - providing a simple interface to load objects from disk *(1)* ;
  - ensuring we don’t try to load the same object twice *(2)* ;
  - resolving dependencies between resources *(3)*.

The first point is obvious, but the two others are less intuitive. *(2)* is
important when the user *might* try to load the same object several times – for
instance, a car model, or a character or a weapon. The most known strategy to
prevent such a situation from happening is by using a *software cache*.

A *software cache* – let’s just say *cache* – is an opaque object that loads the
object on the first request, then just returns that object for future same
requests. For instance, consider the following requests and the corresponding
*cache* behavior:

  1. load "wood.png" -> not cached ; loading ; return
  2. load "grass.png" -> not cached ; loading ; return
  3. load "wood.png" -> cached ; return
  4. load "metal.png" -> not cached ; loading ; return
  5. load "metal.png" -> cached ; return
  6. etc.

That behavior is very nice because it will spare a lot of computations and
memory space.

*(3)* is about dependencies. For instance, when you load a car model, you might
need to load its textures as well. Well, not really *load*. Consider the
following:

  1. load "car.mesh" -> not cached
    1. load "metal_car.png" -> not cached ; loading ; return
    2. loading ; return
  2. load "metal_car.png" -> cached ; return
  3. load "other_car.mesh" -> not cached
    1. load "metal_car.png" -> cached ; return
    2. return
  4. load "car.mesh" -> cached ; return

You got the idea. *(3)* needs *(2)* to be efficient.

## Possible implementations

### Singleton

In imperative languages and especially in those that support template and/or
generics, people tend to implement the cache system with an ugly *design
pattern* – which is actually an *anti design pattern* : *singleton*. Each type
of resource is assigned a manager by using a template parameter, and then if
a manager needs to load a dependency, it just has to reference the corresponding
manager by stating the type in the template parameter :

    Model & getResource<Model>(std::string const &name) {
      Texture &dependency = getResource<Texture>(...);
      ...
    }

That way of doing might sound great, but eh, singletons are just global variables
with a unicity constraint. We don’t want that.

### Explicit pure store

We can use an explicit store object. That is, some kind of *map*. For instance,
the store that holds *textures* would have a type like (in Haskell):

    textureStore :: Map String Texture

A model store would have the following type:

    modelStore :: Map String Model

And each stores is assigned a function; `loadTexture`, `loadModel`, and so on.

There are several drawbacks if we go that way. First, we have to carry all stores
when using their functions. Some functions might need other stuff in order to
resolve dependencies. Secondly, because of explicit state, we need to manually
accumulate state! A loading function would have such a following type:

    loadTexture :: Map String Texture -> String -> m (Texture,Map String Texture)

That will expose **a lot** of boilerplate to the user, and we don’t want that.

### Implicit pure store

We can enhance the explicit store by putting it into some kind of context; for
instance, in `MonadState`. We can then write `loadTexture` to make it nicer to
use:

    loadTexture :: (MonadState (Map String Texture) m,...)
                => String
                -> m Texture

There is a problem with that. What happens when we add more types? For instance if
we want to handle textures **and** models? `MonadState` has a *type family
constraint* that forbids two instances for the pair `s m`. The following is not
allowed and will raise a compiler error:


    instance MonadState (Map String Texture) MyState where
      ...
    
    instance MonadState (Map String Model) MyState where
      ...

The solution to that problem is to have the carried state a polymorphic type and
use typeclass constraint to extract and modify the map we want:

    class HasMap a s where
      extractMap :: s -> Map String a
      modifyMap :: (Map String a -> Map String a) -> s -> s

With that, we can do something like this:

    loadTexture :: (MonadState s m,HasMap Texture s,...)
                => String
                -> m Texture
    
    loadModel :: (MonadState s m,HasMap Texture s,HasMap Model s,...)
              => String
              -> m Model

However, we hit a new issue here. What are `s` and `m`? Well, `m` doesn’t really
matter. For simplicity, let’s state we’re using a *monad transformer*; that is,
we use `StateT s m` as monad.

We still need `s`. The problem is that `s` has to be provided by the user. Worse,
they have to implement **all instances** we need so that the loading functions may
work. Less boilerplate than the explicit store solution, but still a lot of
boilerplate. Imagine you provide a type for `s`, like `Cache`. Expending the
cache to support new types – like user-defined ones – will be more extra
boilerplate to write.

### Closures

The solution I use in my engine might not be the perfect solution. It’s not
[referentially transparent](https://en.wikipedia.org/wiki/Referential_transparency_(computer_science)),
an important concept in Haskell. However, Haskell is not designed to be used in
convenient situations only. We’re hitting a problematic situation. We need to make
a compromise between elegance and simplicity.

The solution required the use of *closures*. If you don’t know what a closure is,
you should check out the [wikipedia page](https://en.wikipedia.org/wiki/Closure_(computer_programming))
for a first shot.

The idea is that our loading functions will perform some `IO` operations to
load objects. Why not putting the cache *directly* in that function? We’d have a
function with an opaque and invisible associated state. Consider the following:

    type ResourceMap a = Map String a

    getTextureManager :: (MonadIO m,...)
                      => m (String -> m Texture)
    getTextureManager = do
      ref <- newIORef empty
      pure $ \name -> do
        -- we can use the ref ResourceMap to insert / lookup value in the map
        -- thanks to the closure!

That solution is great because now, a manager is just a function. How would you
implement `getModelManager`? Well:

    getModelManager :: (MonadIO m,...)
                    => (String -> m Texture)
                    -> m (String -> m Model)
    getModelManager loadTexture = ...

We can then get the loader functions with the following:

    loadTexture <- getTextureManager
    loadModel <- getModelManager loadTexture

And you just have to pass those functions around. The cool thing is that you can
wrap them in a type in your library and provide a function that initializes them
all at once – I do that in my engine. Later on, the user can extend the available
managers by providing new functions for new types. In my engine, I provide a few
functions like `mkResourceManager` that hides the `ResourceMap`, providing two
functions – one for lookup in the map, one for inserting into the map.

# Conclusion

I truly believe that my solution is a good compromise between elegance and ease.
It has a lot of advantages:

  - simple to use ;
  - simple to implement; you just have to play around with closures ;
  - dependencies resolving is easy to add and hidden once the functions are 
    generated ;
  - little runtime overhead (due to closures, might be optimized away by the
    compiler though) ;
  - can be easily extended by the user to support custom/new types ;
  - if correctly used, implementations can replace `IORef` with `TVar` or
    similar objects for thread-safe implementations ;
  - several replicated functions mean several stores (can be useful in certain
    cases).

The huge drawback I see in that solution is its opacity. There’s also no way to
query the state of each cache. Such a feature could be added by proving a new
function, for instance. Same thing for deletion.

I’m one of those Haskellers who love purity. I try to keep my code the purest I
can, but there are exceptions, and that cache problem fits that kind of exception.

Feel free to comment, and as always, keep the vibe and happy hacking!
