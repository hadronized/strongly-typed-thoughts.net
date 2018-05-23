Today, I’ve come across a very interesting problem in Rust. This problem comes from a very specific
part of [luminance].

Rust has this feature called *move semantics*. By default, the *move semantics* are enforced. For
instance:

```
fn foo_move<T>(x: T)
```

This function says that it expects a `T` and need to move it in. If you don’t want to move it, you
have to *borrow* it. Either immutable or mutably.

```
fn foo_borrow<T>(x: &T)
fn foo_borrow_mut<T>(x: &mut T)
```

Now, when you move a value around, it’s possible that you don’t use that value anymore. If you look
at the `foo_move` function, is very likely that we’ll not use that `T` object after the function has
returned. So Rust knows that `x` will go out of scope and then its memory must be invalidated and
made available to future values. This is very simple as it will happen on the stack, but it can get
a bit tricky.

Rust doesn’t have the direct concept of *destructors*. Or does it? Actually, it does. And it’s
called the `Drop` trait. If a type implements `Drop`, any value of this type will call a specific
function for disposal when it goes out of scope. This is akin to running a destructor.

```
struct Test;

impl Drop for Test {
  fn drop(&mut self) {
    println!("drop called!");
  }
}
```

Whenever a value of type `Test` goes out of scope / must die, it will call its `Drop::drop`
implementation and then its memory will be invalidated.

Ok, now let’s get into my problem.

# Moving out of a field

Consider the following code:

```
struct A;

struct B;

struct Foo {
  a: A,
  b: B
}

impl Foo {
  fn take(self) -> (A, B) {
    (self.a, self.b)
  }
}

fn main() {
  let foo = Foo { a: A, b: B };
  let (a, b) = foo.take();
}
```

The implementation of `Foo::take` shows that we *move out of `Foo`* – two, actually. We move the
`a: A` field out of `Foo` and `b: B` out of `Foo`. Which means that we *destructured* `Foo`. The
call to `Foo::take` in the `main` function demonstrates how you would use that function.

Now consider this. `A` and `B` here are opaque type we don’t want to know the implementation. But
keep in mind they represent *scarce resources* – like UUID to important resources we must track in
the `Foo` object. Consider this new code:

```
struct A;

struct B;

struct Foo {
  a: A,
  b: B
}

impl Drop for Foo {
  fn drop(&mut self) {
    // something tricky with self.a and self.b
  }
}

impl Foo {
  fn take(self) -> (A, B) {
    (self.a, self.b) // wait, that doesn’t compile anymore!
  }
}
```

You would get the following error:

> error[E0509]: cannot move out of type `Foo`, which implements the `Drop` trait

As you can see, Rust doesn’t allow you to move out of a value which type implements `Drop`, and this
is quite logical. When `Foo::take` returns, because of `self` going out of scope, it must call
its `Drop::drop` implementation. If you have moved out of it – both `a: A` and `b: B` fields, the
`Drop::drop` implementation is now a complete [UB]. So Rust is right here and doesn’t allow you to
do this.

But imagine that we **have** to do this. For insance, we need to hand over both the scarce resources
`a` and `b` to another struct (in our case, a `(A, B)`, but you could easily imagine a better type
for this).

There’s a way to, still, implement `Foo::take` with `Foo` implementing `Drop`. Here’s how:

```
impl Foo {
  fn take(mut self) -> (A, B) {
    let a = mem::replace(&mut self.a, unsafe { mem::uninitialized() });
    let b = mem::replace(&mut self.b, unsafe { mem::uninitialized() });
    
    mem::forget(self);
    
    (a, b)
  }
}
```

There’s a few new things going on here. First, the `mem::replace` function takes as first parameter
a mutable reference to something we want to replace by its second argument and returns the initial
value the mutable reference pointed to. Because `mem::replace` doesn’t deinitialize any of its
argument, I like to think of that function as a *move-swap*. It just moves out the first argument
and put something in the hole it has made with its second argument.

The `unsafe` function, `mem::uninitialized`, is a bit special as it bypasses Rust’s normal
memory-initialization and actually doesn’t do anything. However, there’s no specification about what
this function really does and you shouldn’t be concerned. What you just need to know is that this
function just gives you *uninitialized* memory.

However, if we just returned `(a, b)` right after the two `mem::replace` calls, the `Drop::drop`
implementation would run at the end of the function, and it would run *badly*, because of the now
uninitialized data you find in the `self.a` and `self.b` fields. It could still be the old values.
Or it could be garbage. We don’t care. We mustn’t read from there. Rust provide a solution to this,
and this is the `mem::forget` function. This function is like dropping your value… without calling
its `Drop::drop` implementation. It’s an escape hatch. It’s a way to invalidate a value and in our
case that function is super handy because `self` now contains uninitialized data!

And here we have a complete working `Foo::take` function.

# So what’s the problem again?

The problem is that I used to function I wish I didn’t:

  - `mem::uninitialized`, which is,
    [as the docs state](https://doc.rust-lang.org/std/mem/fn.uninitialized.html),
    **incredibly hazardous**.
  - `mem::forget`, which is not unsafe in the Rust idea of safety, but still has some kind of very
    nasty unsafety deep in it – it’s very easy to leak resources if you don’t pay extra attention.

The problem is that Rust doesn’t allow you to move out of a value which implement `Drop` even if you
know that you’ll `forget` it. Rust doesn’t currently have a primitive / function to express both
principles at the same time: forgetting and moving out of a `Drop` value.

This had me to come to the realization that I would like an interface like this:

```
fn nodrop<T, F, A>(
  x: T,
  f: F
) -> A
where T: Drop,
      F: FnOnce(T) -> A
```

This function would give you the guarantee that the closure will not `drop` its argument but would
still leak it, temporarily giving you the power to move out of it. You will also notice that if you
want to have `A = T`, it’s possible: that would mean you destructure a value and re-build it back
again inside the same type, which is still sound.

However, this function would make it very easy to leak any field you don’t move out. This is
actually the same situation as with `mem::forget`: if you forget to `mem::replace` a field before
calling `mem::forget`, you end up in the exact same situation.

I agree it’s a very niche need, and if you’re curious about why I need this, it’s because of how
*buffers* are handled in [luminance]. The `Buffer<T>` type has an internal `RawBuffer` as field that
holds GPU handles and internal details and a `PhantomData<T>`. It’s possible to convert from
one type to the other but both implement `Drop`, so you have to implement the hack I just presented
in this blog post to make both conversions. With my `nodrop` function, the code would be way
easier, without needing to unfold `unsafe` blocks. Something like this would be possible:

```
pub fn to_raw(self) -> RawBuffer {
  mem::nodrop(self, |buffer| buffer.raw) // combine mem::forget and mem::replace + uninitialized
}
```

What do you peeps think of such a function? For sure that would add possible easy leaks, that’s a
drawback I find very difficult not to agree with. However, I just wanted to share my little
experience of Tuesday evening.

That’s all for me. Thanks for having read me, I love you. Keep the vibes!

[luminance]: https://crates.io/crates/luminance
[UB]: https://en.wikipedia.org/wiki/Undefined_behavior
