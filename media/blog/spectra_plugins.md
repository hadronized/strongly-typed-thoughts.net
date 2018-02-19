# Spectra gets Rust scripting!

[spectra] is a crate I’ve been maintaining for a few months / years now. It’s a crate that I mainly
use for demoscene productions (I released two with it, [Céleri Rémoulade] and [Outline Invitation])
but I also use it to play around and experiment new rendering, animation and video game techniques.

The crate features a few things that I enjoy daily. Among them:

- A rendering layer, backed by another crate of mine, [luminance].
- Hot reloading for scarce resources – the feature was extracted into [warmy] as folks on IRC asked
  me to!
- Models (.OBJ) loaders.
- Audio simple routines (basically: play a soundtrack and get metadata about it).
- A shading language of mine, built upon GLSL (I’ll make a separate blog entry about this one).
- Splines, animation primitives, etc.
- And more.

The last thing I’ve been working on is *being productive*. That might seem counter-intuitive, but
when you start building *“frameworks”* and *“engines”*, you actually end up writing a lot of code
for *“the beauty and power of it”* and lose focus on what’s important: releasing things. I know
that and looked for what I could enhance to augment my productivity.

Among interesting topics I came up with, I stated:

- JSON live editing with [warmy] in [spectra] is totally awesome (it was an improvement for my
  productivity when I was, before, editing the curves in the code – you know, the edit-compile-run
  cycle?). However, even if it’s awesome, it’s quickly limiting, especially when you want to edit
  quaternions (rotations) or find a specific color mask. So I’d like to implement a way to pick
  objects around with the mouse and edit them via a nice UI.
- Scripting. Somewhat. I partially done that with hot-reloaded shaders!
- Better tooling, especially for live-coding.

Clearly, there’s something about live coding.

# Introducing scripting in Rust

I went through several thought processes. I first had a look at [Lua], because lots of people think
it’s cool. However, I don’t like the syntax nor the overall safety the language
<strike>gives</strike> *doesn’t* give you.

I remembered that almost a decade ago, when I was 15 or 16 year old, I implemented some kind of a
*plugins* system in C++ for a side project of mine. The idea was simple:

- Find functions with `dlopen`, `dlsym` to open a relocatable object / archive (.so / .dll).
- Use them at runtime.

This is not magic. When you link a crate / library, by default, a lot of compilers will perform
*static linking*. That’s the case of [ghc] or [rustc], in most situations.

> If you don’t know yet, a library / crate / dependency statically-linked in a program means that
> all of its symbols (the ones used in the program at least) are placed in a specific section of
> the generated executable, so that those symbols have a proper address and “come with the binary”.

Dynamic linking, on the other side, is a way to express a *dependency* between your binary and some
code, which is most of the time living in a relocatable archive (`.so` on Linux or macOSX, `.dll` on
Windows – macOSX also uses `.framework` but it’s just for the overall idea). When your binary needs
to call a function defined in a dynamically linked library, it’ll open the shared library with the
`dlopen` function, try to locate the function by giving its (unmangled) name to `dlsym` or `dladdr`
for instance, if the function exists, you’ll be able to run its code.

There exists attacks and interesting things you can do with dynamic libraries. Because the code
doesn’t live in the binary, you can for instance replace a legit and safe `.dll` on Windows with a
malicious one. Or you can patch a buggy dynamic library by shipping a new `.dll` / `.so` without
having its dependent to re-compile their applications (which would be needed in case of static
linking). Another funny thing you can do: [fraps], a real-time recorder for your video games,
performs some kind of `.dll` injection by pushing an OpenGL `.dll` into your binary (Windows allows
this) and replacing some known functions. For instance, the function responsible in swapping your
renderbuffer chains. It can then intercepts pixels regions, adds overlay, etc. Fascinating! ;)

Anyway, the idea is that whenever you link a dynamic library, your compiler / linker will just
insert the required code in your binary (think of `dlopen`) so that your binary can load code at
runtime. It’s then pretty easy to implement a plugin system:

- Define your interface. Define a function type you will need to take from the dynamic library and
  give that interface a name so that you can look it up in the library.
- Open the library, find the symbol, and just use it!

So you could imagine, as a very simple start example, a `loop` that would simply invokes such a
function. Whenever you change the library, the code getting ran will automatically changes as well!

## Scripting in spectra

So I decided to reproduce that in Rust using the few crates of mine:

- `spectra`: obviously, since it’s the crate that will receive the feature first.
- `warmy`: it’s indirect since `spectra` re-exports it and adds a few things over it, like a
  *resource* system (error messages, timing loading and reloading, etc.).
- [`libloading`]: the *Rust way* to go with dynamic libraries (it has an `unsafe` interface though).
- That’s pretty much everything of it.

> For unix plateforms, you can see that `libloading` is just a smart wrapper over the functions I
> mentionned. [See for yourself](https://github.com/nagisa/rust_libloading/blob/master/src/os/unix/mod.rs#L281).

Ok, so, let’s try the experiment!

# Let’s write plugins in Rust!

## Foreword

The first thing we must accomplish is very simple: we want to be able to load some (Rust) code at
runtime, inside our application. For achieving that goal, we need to load a dynamic library (`.so`
on Linux) with [`libloading::Libray::open`](https://docs.rs/libloading/0.5.0/libloading/struct.Library.html#method.new).
Once we have the library, we can just look a symbol up with
[`libloading::Library::get`](https://docs.rs/libloading/0.5.0/libloading/struct.Library.html#method.get).
In case of a successful lookup, that function returns a value of type `Symbol>`, which implements
`Deref` for the symbol your asking.

> Dynamic library typically gathers functions, so we’ll be looking for `fn` symbols.

However, we don’t have any dynamic library yet. We only have… a Rust file – `.rs`. How can we get
a dynamic library out of it?

## Generating a dynamic library

This is actually pretty simple. We’re gonna start easily by making a `.so` that will just contain
a function called `hello_world` that will just print `"Hello, world!"` on `stdout`. You all know how
to implement such a function:

```
/// hello_world.rs
pub fn hello_world() {
  println!("Hello, world!");
}
```

> We make the function `pub` so that it gets actually exported when we generate the dynamic library.

Copy that code and put it in, for instance, `/tmp`.

Then, let’s generate a dynamic library!

```
cd /tmp
rustc --crate-type dylib hello_world.rs
```

Once `rustc` returned, you should see a new file in `/tmp`: `/tmp/libhello_world.so`! Here we are!
We have a dynamic library! Let’s try to find our function in it with the handy `nm` program.

> I reckon `nm` is already installed on your machine. If not, it should come with packages like
> `base-devel`, `build-essentials` or that kind of meta packages.

```
nm /tmp/libhello_world.so | grep hello_world
0000000000000000 N rust_metadata_hello_world_8787f43e282added376259c1adb08b80
0000000000040ba0 T _ZN11hello_world11hello_world17h49fe1e199729658eE
```

Urgh. We have a problem. Rust has mangling for its symbol names. It means that it will alter the
symbols so that it can recognize them in a dynamic library. For instance, the mangle version of a
function name `foo` defined for a type `A` won’t be the same as the one of `foo` defined for a type
`B`. However, in our case, we don’t want mangling, because, well, we won’t be able to lookup the
name up – no one will even try to guess that `_ZN11hello_world11hello_world17h49fe1e199729658eE`
function name.

`rustc` has a very simple solution to that: just tell it you don’t want a symbol’s name to be
mangled. It’ll be stored in the dynamic library the way you write it. This is done with the
`#[no_mangle]` attribute.

```
/// hello_world.rs
#[no_mangle]
pub fn hello_world() {
  println!("Hello, world!");
}
```

Now recompile with the same `rustc` line, and run the `nm` + `grep` oneliner again.

```
nm /tmp/libhello_world.so | grep hello_world
0000000000040b70 T hello_world
0000000000000000 N rust_metadata_hello_world_8787f43e282added376259c1adb08b80
```

Now you can see there exists a symbol called `hello_world`: this is our symbol!

## Load the library in Rust and read a symbol

For our little example here, we’re just gonna load and run the code in a new project.

```
cd /tmp
cargo new --bin dyn-hello-world
     Created binary (application) `dyn-hello-world` project
cd dyn-hello-world
```

Edit the `Cargo.tml` file to include `libloading = "0.5"` in the `[dependencies]` section. Ok, we’re
good to go. Run a second terminal in which you run this command to automatically check whether your
code is okay:

```
cd /tmp/dyn-hello-world
cargo watch
```

Let’s edit our `main.rs` file.

```
extern crate libloading;

use libloading::Library;

fn main() {
  let lib = Library::new("/tmp/libhello_world.so").unwrap();
  let symbol = unsafe { lib.get::<extern fn ()>(b"hello_world").unwrap() };

  symbol();
}
```

It should check. A bit of explanation:

- The `Library::new` accepts as first and only argument the path to the dynamic library. In our
  case, we just use our freshly generated `libhello_world.so`.
- The `unsafe` `get` call takes the symbol we look for as argument and returns it if it’s found it.
- Because `Symbol` implements `Deref`, here, we can directly call the function with `symbol()`.

Now compile and run the code:

```
cargo build
   Compiling cc v1.0.4
   Compiling libloading v0.5.0
   Compiling dyn-hello-world v0.1.0 (file:///tmp/dyn-hello-world)
    Finished dev [unoptimized + debuginfo] target(s) in 2.63 secs
cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/dyn-hello-world`
Hello, world!
```

Hurray! We’ve just built our first dynamic library loader!

## Let’s compile Rust… from Rust!

Ok, now, let’s iterate: we need to compile the Rust code from our Rust code (haha). For doing that,
we’ll need to generate the dynamic library at some place. Because I don’t like to put artifacts in
random places, I like to use the `tempdir` crate, which gives you a `TempDir` that creates a new
temporary directory with a random name when you ask for it, and removes it from your filesystem when
the `TempDir` goes out of scope. We’ll also be using the `std::process` module to run `rustc`.

Add the following to your `[dependencies]` section in your `Cargo.toml`:

```
tempdir = "0.3"
```

Ok, let’s compile a Rust source file into a dynamic library from our Rust source code!

```
extern crate libloading;
extern crate tempdir;

use libloading::Library;
use std::process::Command;
use std::str::from_utf8_unchecked;
use tempdir::TempDir;

fn main() {
  let dir = TempDir::new("").unwrap(); // we’ll drop the .so here
  let target_path = dir.path().join("libhello_world.so");
  
  let compilation =
    Command::new("rustc")
    .arg("--crate-type")
    .arg("dylib")
    .arg("/tmp/hello_world.rs")
    .arg("-o")
    .arg(&target_path)
    .output()
    .unwrap();

  if compilation.status.success() {
    let lib = Library::new(&target_path).unwrap();
    let symbol = unsafe { lib.get::<extern fn ()>(b"hello_world").unwrap() };

    symbol();
  } else {
    let stderr = unsafe { from_utf8_unchecked(compilation.stderr.as_slice()) };
    eprintln!("cannot compile {}", stderr);
  }
}
```

Compile, run, and…

```
cargo run
   Compiling dyn-hello-world v0.1.0 (file:///tmp/dyn-hello-world)
    Finished dev [unoptimized + debuginfo] target(s) in 0.58 secs
     Running `target/debug/dyn-hello-world`
Hello, world!
```

This piece of code is the first premise of our plugin system. You can see interesting properties:

- You don’t have to re-compile `dyn-hello-world` to change its behavior, since it comes from a
  dynamic library.
- There’s no need to depend on a dynamic library *directly*, since we can just generate it on the
  fly!
- Can display error messages (the current code is a bit perfunctory but you could enhance to have
  nice error messages!).

However, there’s a problem. Try adding an `extern crate` to `hello_world.rs`, like, for instance:

```
extern crate spectra;

#[no_mangle]
pub fn hello_world() {
  println!("Hello, world!");
}
```

Run `dyn-hello-world`.

```
cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/dyn-hello-world`
cannot compile error[E0463]: can't find crate for `spectra`
 --> /tmp/hello_world.rs:1:1
  |
1 | extern crate spectra;
  | ^^^^^^^^^^^^^^^^^^^^^ can't find crate

error: aborting due to previous error
```

Damn, how are we gonna solve this?

## Finding dependencies!

The key is to understand how `cargo` deals with the dependencies you declare in your `Cargo.toml`’s
`[dependencies]` section. To do this, clean your project, and recompile in *verbose* mode – we’re
like… *reverse engineering* `cargo build`!

```
cargo clean -p dyn-hello-world
cargo build --verbose
       Fresh libc v0.2.36
       Fresh winapi-build v0.1.1
       Fresh winapi v0.2.8
       Fresh cc v1.0.4
       Fresh rand v0.4.2
       Fresh kernel32-sys v0.2.2
       Fresh libloading v0.5.0
       Fresh remove_dir_all v0.3.0
       Fresh tempdir v0.3.6
   Compiling dyn-hello-world v0.1.0 (file:///tmp/dyn-hello-world)
     Running `rustc --crate-name dyn_hello_world src/main.rs --crate-type bin --emit=dep-info,link -C debuginfo=2 -C metadata=e8b1e5e96f709abe -C extra-filename=-e8b1e5e96f709abe --out-dir /tmp/dyn-hello-world/target/debug/deps -C incremental=/tmp/dyn-hello-world/target/debug/incremental -L dependency=/tmp/dyn-hello-world/target/debug/deps --extern tempdir=/tmp/dyn-hello-world/target/debug/deps/libtempdir-9929bcad6dc8cc47.rlib --extern libloading=/tmp/dyn-hello-world/target/debug/deps/liblibloading-a2854ce154eb4d6f.rlib -L native=/tmp/dyn-hello-world/target/debug/build/libloading-4553b9f132aa813a/out`
    Finished dev [unoptimized + debuginfo] target(s) in 0.50 secs
```

We can see a few things going on here. First, there are some `Fresh` lines we’re not interested
about. Then `cargo` tries to compile our application. You can see an invokation to `rustc` with a
long list of arguments. Among them, two interest us:

- The one specifying the list of dependencies your project, needs:
  + `-L dependency=/tmp/dyn-hello-world/target/debug/deps`.
- The ones specifying the list of crates you might use in `extern crate` statements.
  + `--extern tempdir=/tmp/dyn-hello-world/target/debug/deps/libtempdir-9929bcad6dc8cc47.rlib`
  + `--extern libloading=/tmp/dyn-hello-world/target/debug/deps/liblibloading-a2854ce154eb4d6f.rlib`

> There’s also a `-L native=…` one. This is explained in the manual of `rustc` and corresponds to
> native library we must link against, like a C library, for instance.

As you can see, the path used in `-L dependencie=…` is pretty *constant*. It seems it has the form:

```
/path/to/project/target/<build type>/deps
```

However, remember that the initial intent was to write a plugin system for `spectra`, which is a
library. We don’t know the path to the project that will be using `spectra`, so we must find it in
a way.

Two options:

- We could use a *macro* (`macro_rules`) to automatically insert it at the calling site. However,
  I’m not even sure it would work (you need to find a way to have access to `cargo`’s internal…
  maybe via the `env!` macro?).
- We could find the path at runtime.

I chose the second option because it was pretty easy and straight-forward to implement. However,
I’m not a huge fan of it – it’s pretty… non-elegant to me. Here’s the code that gives me the root
path of the current project a function is defined in:

```
/// Try to find the project root path so that we can pick dependencies.
fn find_project_root() -> Result<PathBuf, PluginError> {
  let result =
    Command::new("cargo")
    .arg("locate-project")
    .output()
    .unwrap();

  if !result.status.success() {
    Err(PluginError::CannotFindMetaData("cannot locate root project".to_owned()))
  } else {
    let json = unsafe { from_utf8_unchecked(result.stdout.as_slice()) };
    let root =
      json.split(':').nth(1).and_then(|x| {
        if x.len() >= 3 {
          Path::new(&x[1..x.len()-3]).parent().map(|x| x.to_owned())
        } else {
          None
        }
      });

    root.ok_or_else(|| PluginError::CannotFindMetaData("cannot extract root project path from metadata".to_owned()))
  }
}
```

As you can see, I use the `cargo locate-project` feature, that gives you the root path of the
project the `cargo` invokation is in. It supports calling it from a subdirectory, which is neat (a
bit like `git`, actually). Most of the code is removing the JSON sugar over it. It’s pretty unsafe
because if the output format of `cargo locate-project` changes, this code will basically break.

> Pro reminder for myself: write unit tests for that piece of code. :D

I won’t show the `PluginError` type, it’s not important and it’s `spectra` current code for the
feature.

Ok, we lack two things:

- The build type.
- The path to our crate – here, `spectra`.

Finding the build type is pretty easy: you can use the `debug_assertions` `cfg!` argument. It’s set
to `true` on `debug` target and `false` on `release`. You can actually witness it works with the
following oneliner:

```
println!("build target is debug: {}", cfg!(debug_assertions));
```

Ok, now, how do we find our `spectra` crate’s path? You saw it has a metadata glued to its name in
the previous verbose output.

I didn’t find any satisfying solution. I just came up with *a solution*. I warn you: it’s not
elegant, it’s highly unsafe, but for now, it works. I’ll try to find a better way later.

Here’s the code.

```
/// Find the spectra crate in the given directory.
fn find_spectra_crate(path: &Path) -> Result<PathBuf, PluginError> {
  // we open the directory we pass in as argument to the function
  if let Ok(dir) = path.read_dir() {
    // we iterate over all of its contained files to find if it has our spectra crate
    for entry in dir {
      let path = entry.unwrap().path();

      match path.file_name() {
        // we try to pattern match its name; this is a bit unsafe because it won’t support two
        // versions of libspectra… meh.
        Some(filename) if filename.to_str().unwrap().starts_with("libspectra") => {
          return Ok(path.to_owned());
        }

        _ => ()
      }
    }

    Err(PluginError::CannotFindMetaData("cannot find the spectra crate; try to build your project’s dependencies".to_owned()))
  } else {
    Err(PluginError::CannotFindMetaData("cannot find read dependencies".to_owned()))
  }
}
```

If you put those three functions altogether, you can now implement the `rustc` call without
hardcoding any paths, since they all will be found at runtime and injected in the call!

## How do you make auto-reload again?

I didn’t speak about that, but in `spectra`, it’s **very easy** to have a resource to auto-reload if
it changes on the disk. This is done via the `warmy` crate. You just implement the
[`Load`](https://docs.rs/warmy/0.5.2/warmy/trait.Load.html) trait and ask for your type at a
[`Store`](https://docs.rs/warmy/0.5.2/warmy/struct.Store.html) by providing a typed key. I won’t
talk too much about `warmy` – if you’re interested, go read the online documentation
[here](https://docs.rs/warmy)!

The idea is that I just created a type that wraps both `libloading::Library` and `tempdir::TempDir`.
Something like this:

```
pub struct Plugin {
  lib: Library,
  dir: TempDir
}
```

I still don’t know whether I need to implement `Drop` or not – if the `TempDir` dies first, I don’t
know whether the `Library` object is in an unstable state. If not, that means that the whole library
was loaded in RAM at the end of the `Library::new` call, and that I don’t even need to keep the
temporary directory around!

## It’s just the beginning

The interface of a `Plugin` is not yet defined. I’m writing this blog entry on Sunday night / Monday
morning, while I had that plugin experiment over the weekend. A few thoughts, though:

- My current code enables me to run my demo (graphics-ish) and edit Rust code to script it *live*,
  which is completely amazing to me!
- It should be easy to expose a safe interface by asking the plugin to expose a normal function and
  inject a header in the Rust code that contains an `unsafe fn`, calling the safe function or a
  trait’s method or whatever.
- You have no idea how much I’m excited about all this!

I have a *lot* of things to say, especially lately. I’ll be posting more thoughts and experiments of
mine soon! Thanks for having read through, and as always, keep the vibes!

[spectra]: https://crates.io/crates/spectra
[Céleri Rémoulade]: https://www.youtube.com/watch?v=pYqZS1C_7PU
[Outline Invitation]: https://www.youtube.com/watch?v=OemyLQbDTSk
[luminance]: https://crates.io/crates/luminance
[warmy]: https://crates.io/crates/luminance
[Lua]: https://www.lua.org
[ghc]: https://www.haskell.org
[rustc]: https://www.rust-lang.org
[fraps]: http://www.fraps.com/
[`libloading`]: https://crates.io/crates/libloading
