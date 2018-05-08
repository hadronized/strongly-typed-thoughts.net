Last weeks were interesting for [warmy], a crate I’ve been writing for several weeks / months
now that enables you to hot load and reload scarce resources – e.g. textures, meshes, configuration,
JSON parameters, dependency nodes, whatever. [warmy] received several interesting features, among:

- *Context passing*: it is now possible to pass a mutable reference (`&mut`) to a typed context
  when loading and reloading a resource. This enables per-value loadings, which is neat if you need
  to add extra data when loading your resources (e.g. increment a counter).
- *Methods support*: before version 0.7, you had to implement a trait, `Load`, in order to tell
  [warmy] how to load a given object of a given type. This is convenient but carries a bad drawback:
  if you want to represent your object with both JSON and XML for instance, you need to *type wrap*
  your type so that you can `impl Load` twice. This was annoying and the type system also handed you
  back an object which type was the wrapper type, not the wrapped type. This annoyance was
  removed in 0.7 as you now have an extra type variable to `Load` – it defaults to `()` though –
  that you can use to `impl Load` several times – think of that tag type variable as a type
  representing the *encoding* or the *method* to use to load your value.
- *A VFS (Virtual FileSystem)*: the VFS makes it possible to write resource keys without caring
  about their real location – e.g. `/splines/color_grading.json`. Before that, you still had to
  provide a real filesystem path, which was both confusing and annoying (since you already give one
  when you create a `Store`, the object that holds your resource).
- [Changelog here](https://github.com/phaazon/warmy/blob/master/CHANGELOG.md#07).

[I posted on reddit](https://www.reddit.com/r/rust/comments/8fy3q4/warmy070_vfs_context_passing_reload_methods_and/)
in order to make people know of the new version, and interesting talks started to occur on both IRC
and GitHub. What people seem to want the most now is *asynchronous loading and reloading*. I’ve been
wanting that feature for a while too so I decided it could be a good idea to start working on it.
However, after a day of toying around, I came to the realization that I should write a small blog
post about it because I think it’s not trivial and it could help me shape my ideas.

> Note: this post is about brainstorming and setting up the frame to why and how async warmy. You
> will find incomplete code, ideas and questions there. If you want to contribute to the discussion,
> you’re more than welcome!

# Synchronous versus asynchronous

What does it mean to have a *synchronous* computation? What does it mean to have an *asynchronous*
one? You might find it funny, but a lot of people are still confused with the exact definition, so
I’ll try to give you more hindsight.

We talk about a *synchronous* task when we have to wait for it to finish before moving on to another
task. We have to wait until its completion to get the control flow back and call other functions. We
often talk about *blocking computations*, because you cannot do anything else while that computation
is running – at least on the thread this computation is running on.

We talk about an *asynchronous* task when you can get the control flow back without having to wait
for the task to finish. However, **that doesn’t necessarily mean that the task is being executed in
parallel or concurrently**. At some extent, you could easily label [generators] as *asynchronous
primitives*, and this is what actually happens in `async / await` code: you give back the control
flow to the caller and the callee execution gets re-scheduled later. Hence, this is asynchronous
programming, yet the scheduling execution could be implemented on a single thread – hence no
parallel nor concurrency actually happen. Another example is when you perform a HTTP request: you
can send the request and instead of blocking until the response arrive, you can give the control
back, do something else, and then, at some time, handle the response. You don’t need parallelism
to do this: you need asynchronous programming.

> Note: a generalization of a generator is a [coroutine], which hands the control flow back to
> another [coroutine] instead of the caller when wanted.

# What about warmy?

At the time of writing this blog entry, [warmy] is completely synchronous. Consider the following
example:

```
extern crate serde_json;
extern crate warmy;

use std::error;
use std::fmt;
use std::fs::File;
use std::io;
use std::thread;
use std::time::Duration;
use warmy::{FSKey, Load, Loaded, Res, Store, StoreOpt, Storage};

struct JSON;

#[derive(Clone, Copy, Debug, PartialEq)]
struct Foo {
  array: [f32; 4]
}

#[derive(Debug)]
enum FooError {
  JsonError(serde_json::Error),
  IOError(io::Error)
}

impl fmt::Display for FooError {
  fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
    match *self {
      FooError::JsonError(ref e) => e.fmt(f),
      FooError::IOError(ref e) => e.fmt(f),
    }
  }
}

impl error::Error for FooError {
  fn description(&self) -> &str {
    match *self {
      FooError::JsonError(ref e) => e.description(),
      FooError::IOError(ref e) => e.description(),
    }
  }

  fn cause(&self) -> Option<&error::Error> {
    match *self {
      FooError::JsonError(ref e) => Some(e),
      FooError::IOError(ref e) => Some(e),
    }
  }
}

impl<C> Load<C, JSON> for Foo {
  type Key = FSKey;

  type Error = FooError;
  
  fn load(
    key: Self::Key,
    _: &mut Storage<C>,
    _: &mut C,
  ) -> Result<Loaded<Self>, Self::Error> {
    let file = File::open(key.as_path()).map_err(FooError::IOError)?;
    let array = serde_json::from_reader(file).map_err(FooError::JsonError)?;
    let foo = Foo { array };

    Ok(foo.into())
  }
}

fn main() {
  // read scarce resources in /tmp
  let ctx = &mut ();
  let opt = StoreOpt::default().set_root("/tmp");
  let mut store: Store<()> = Store::new(opt).expect("store creation failed");

  // get our resources via JSON decoding
  let foo: Res<Foo> = store.get_by(&FSKey::new("/foo.json"), ctx, JSON).expect("foo should be there");
  let foo2: Res<Foo> = store.get_by(&FSKey::new("/foo2.json"), ctx, JSON).expect("foo should be there");

  println!("{:#?}", foo);
  println!("{:#?}", foo2);

  loop {
    // sync the resources with the disk
    store.sync(ctx);
    thread::sleep(Duration::from_millis(100));
  }
}
```

> Note: you can test that code by inserting `warmy = "0.7"` and `serde_json = "1"` in a `Cargo.toml`
> and by creating the JSON files containing 4D arrays of numbers, like `[0, 1, 2, 3]` in
> `/tmp/foo{,2}.json`.

If you look closely at that example, you can spot two important locations when we get resources: when
we create the `foo` and `foo2` bindings (search for `store.get_by` invocations). Here, it’s important
to understand what’s really happening:

  1. First, `/foo.json` is loading on the current thread.
  2. Then, when it’s loaded and cached, `/foo2.json` starts loading.

We can already see a pity here: both the files are completely disconnected from each other, yet, the
second file must wait for the former to finish before starting loading. We’re not using all the
cores / threads of our CPU. So we could perfectly imagine this new scenario:

  1. First, ask to load `/foo.json` and immediately get control flow back.
  2. Then, ask to load `/foo2.json` and immediately get control flow back.
  3. Wait for both the resources to be available, then continue.

You could even do whatever you want between (2.) and (3.). The point here is that we can run tasks
without having to wait for them to finish before starting others. The explicit *waiting* part could
be a blocking call, such as:

```
let foo_task = store.get_by(…);
let foo2_task = store.get_by(…);

let foo = foo_task.wait();
let foo2 = foo2_task.wait();
```

> Note: this is not the foreseen or planned interface. It’s just there to illustrate what we want to
> achieve there.

The goal of this blog entry is to explore possible implementations.

# futures

The [futures] crate is a very promising and interesting crate. What it provides is basically a mean
to express *data that might get available in the future*. For instance:

```
fn ping(host: &str) -> ???
```

Here, `ping` is a function that will perform an [ICMP] request to a given host, identified by the
function argument. Because that function might take a while (at least several milliseconds, which is
**a lot**), we have to make a decision:

  1. Either we decide to block the current thread until a response gets available (the host responds
     to our ping and we get the packet back).
  2. Or either we decide to free the control flow from the `ping` function and do something else
     until the response arrive.

If you’ve followed all what I said since the beginning of this blog post, you might have noticed that
(1.) is synchronous and (2.) is asynchronous (also notice that we haven’t talked about parallelism
yet!).

With (1.), we would write `ping`’s signature like this:

```
fn ping(host: &str) -> Result<PingResponse, NetworkError>
```

With (2.), we would write it like this, using the `futures` crate:

```
fn ping(host: &str) -> impl Future<Item = PingResponse, Error = NetworkError>
```

> Note: the syntax `impl Trait` is called *conservative impl trait* and its description can be found
> in the [corresponding RFC on impl Trait].

So what you basically do here is to send a non-blocking request and get its result in a non-blocking
way. Sweet! How does that apply to [warmy]?

# futures applied to warmy

Asynchronous [warmy] should have the following properties:

  - When you ask for a resource to load, you immediately get the control flow back.
  - Hot-reloading mustn’t block either, as you will do it in the `Store::sync` function.
  - Because of the asynchronous nature of those computations, the *context passing* now gets a bit
    tricky: we cannot handle a mutable reference on our context anymore, because otherwise we would
    block the main thread. We need to share the context in a smart way.
  - Finally, all parallel computations should be ran on some kind of *green thread* so that we don’t
    have to worry about destroying the OS threads amount.

## Asynchronous loading

The [current definition of the `Store::get` function](https://docs.rs/warmy/0.7.2/warmy/load/struct.Storage.html#method.get)
is:

```
pub fn get<K, T>(
  &mut self, 
  key: &K, 
  ctx: &mut C
) -> Result<Res<T>, StoreErrorOr<T, C>>
where
  T: Load<C>,
  K: Clone + Into<T::Key>
```

We want something like this:

```
pub fn async_get<K, T>(
  &mut self, 
  key: &K, 
  ctx: &mut C
) -> Result<impl Future<Item = AsyncRes<T>, Error = StoreErrorOr<T, C>>, StoreErrorOr<T, C>>
where
  T: Load<C>,
  K: Clone + Into<T::Key>
```

Woosh. It gets hard to read – eventually, [trait aliases] will help us there. We can see the use of
the `futures` crate here, in the return type:

```
impl Future<Item = AsyncRes<T>, Error = StoreErrorOr<T, C>>
```

Which reads as *“An asynchronous resource of type `T` available in the future or an error due to the
loading of `T` and `C` or due to the store”*.

However, something important to notice is that we miss a crucial point here: we actually *want a
parallel execution*. We could start try by defining a small state machine to step through the
process of loading a resource. That could be:

```
enum LoadState<K, T, C> {
  Pending(K, Shared<C>), // resource loading pending
  Loading(Receiver<AsyncRes<T>>), // resource currently loading
  Done, // resource loaded
}
```

This small state machine can be stepped through by the following process:

```
fn start_load<K, T, C>(key: K, ctx: &mut C) -> impl Future<Item = LoadState<K, T, C>> {
  LoadState::Pending(key, ctx.share_somehow())
}

impl<K, T, C> Future for LoadState<K, T, C> {
  type Item = AsyncRes<T>;

  type Error = StoreErrorOr<T, C>;

  fn poll(&mut self, ctx: &mut Context) -> Result<Async<Self::Item>, Self::Error> {
    match *self {
      LoadState::Pending(..) => {
        // start the loading
        let (sx, rx) = oneshot::channel();
        let task = Task::new(move || {
          // load the resource somehow
          let res = …;
          sx.send(res);
        });

        // spawn a new task to load the resource and update the state machine
        ctx.spawn(task);
        *self = LoadState::Loading(rx);
        Ok(Async::NotReady)
      }

      LoadState::Loading(ref mut rx) => {
        match rx.map(|res| {
          *self = LoadState::Done;
          res
        }).poll()?
      }

      LoadState::Done => panic!("poll called after future has arrived")
    }
  }
}
```

That code might be incomplete or not typecheck completely (not tested), but the idea is there. I
still don’t know whether this is the good way to use `futures` or whether I should run a single, long
computation without the small state machine. I just don’t know yet.

## Asynchronous reloading

This part is tightly bound to how the `Store::sync` will behave. Now that I tink about it, maybe it’d
be more ergonomic / elegant / simpler to have an event loop living on a separate thread that would
benefit from `epoll` and all those fancy asynchronous primitives to deal with that. That would result
in no more by-hand synchronization, since it’d be done by the event loop / reactor / whatever you
want to name it.

Currently, I have not settled up on any decision regarding reloading.

## Asynchronous context passing

This will be definitely a tricky part as well, because what was before:

```
store.sync(&mut ctx);
```

will then become something like:

```
store.sync(ctx.share());
```

Also, something that might happen is that because the synchronization will be done in an asynchronous
way, you will need to ensure that you do not try to synchronize the same resource several times.

Finally, if you take into account the previous section, the user might not even synchronize by hand
anymore, so the context will have to be shared anyway and moved to the event loop. Argh. That doesn’t
seem simple at all! :)

## Parallel execution

One final aspect of asynchronous [warmy] is obviously how we’re going to `poll` the various `Future`s
generated by the `get` functions. One first, naive idea would be to run those in OS threads. However,
imagine that you’ll be loading plenty of resources (on a big and release project, it’s easy to
imagine several thousands of resources loading in parallel). You don’t want to allocate that many OS
threads but instead rely on a green threading solution… which I haven’t found yet. People on IRC
advised to have a look at [futures-cpupool], but I’d like to have a complete solution set before
deciding anything.

I will post this blog post on reddit with hope that people will come up with ideas and I’m sure
enlighting ideas on how to cope with all of this asynchronous properties I want to push to [warmy].

Thanks for having read me, and keep the vibes!

[warmy]: https://crates.io/crates/warmy
[generators]: https://en.wikipedia.org/wiki/Generator_(computer_programming)
[coroutine]: https://en.wikipedia.org/wiki/Coroutine
[futures]: https://crates.io/crates/futures
[futures-cpupool]: https://crates.io/crates/futures-cpupool
[ICMP]: https://en.wikipedia.org/wiki/Internet_Control_Message_Protocol
[corresponding RFC on impl Trait]: https://github.com/rust-lang/rfcs/blob/master/text/1522-conservative-impl-trait.md
[trait aliases]: https://github.com/rust-lang/rfcs/blob/master/text/1733-trait-alias.md
