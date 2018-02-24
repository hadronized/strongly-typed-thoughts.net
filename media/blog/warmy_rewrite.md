# warmy-0.6.0

Today I released a new version of [warmy], the `warmy-0.6.0` release. That release kicks in with a
few additions, among:

- Complete rewrite of its internals, enabling optimizations (mostly about allocations).
- A nasty IO-bound bug was removed.

That bug caused long-lasting reloads on stream-copied resources go into weird behavior.

# The bug

In order to get the bug, I must give a bit of context.

Imagine you have a large resource, like a 4K texture or a big mesh. Whenever your favourite
software writes it to disk, it’s very likely it’ll *stream* chunks by chunks of bytes. For
instance, it might choose to copy the resource *2 MB* by *2 MB* on disk. On each copy, your
file system will generate `WRITE` events, that [warmy] will intercept. Before `warmy-0.6.0`, the
default behavior was to reload the resource on the first `WRITE` event, which was already
wrong, because only a very small part of the resource would have changed – I actually witnessed
weird behaviors with textures in a demo of mine I’m working on, not seeing the new textures in my
demo.

But there’s worse. There’s a parameter you can set of your `Store`, called `update_await_time_ms`.
That parameter gives [warmy] a hint about how much time must have passed since the last update in
order to effectively call the [`Load::reload`] function. However, this is a bit twisted, because if
the resource takes more time to reload than `update_await_time_ms`, it’ll get repeatedly loaded –
for as many as `WRITE` events were generated. This is a bit sick, yeah.

# The fix

The fix was pretty simple: change the semantics of that `update_await_time_ms`. In `0.5.2`, it has
the default value of **1s**, meaning that a resource wouldn’t reload if it was reloaded less than a
second ago. The new semantics works on the future. Whenever a `WRITE` event is intercepted, [warmy]
will call the [`Load::reload`] function only if no `WRITE` event is intercepted in the next
`update_await_time_ms`. It’s a bit like the implementation of a click and a double click: you must
wait a bit after you got a `MouseRelease` event in order to interpret is as a `Click` because
another `MouseRelease` could arrive soon (generating a `DoubleClick` if it’s soon enough).

> You’ll also notice that `update_await_time_ms` name sticks better to the new semantics!

In `0.5.2`, the default value for `update_await_time_ms` was **1s**. If we kept that value, it would
result in a pretty bad overall *latency*. The value was lowered to **50ms** instead.

> You can still tweak that value if it doesn’t suit your needs.

More information can be found in the [changelog].

# On the future of the crate

I’ve been very happy with what [warmy] has brought to me so far. Other people also gave it a try
and for now seem to enjoy it. I’ve gathered a few ideas for the future, based on IRL talks over 
<strike>a beer</strike> several beers, and GitHub issues / pull requests:

- We want to be able to pass around a *context* when loading and reloading. This would enable
  tweaking the loading behavior of the objects and pass values from the calling function. However,
  even though the feature is well-identified, it is not well-defined in [warmy] and more research
  must be done.
- Multithreading. There’re a few ideas I’d like to implement, like an IO thread that would do all
  the file IO-bound computation and dispatch the bytes to the other threads; and using `Future`.

Feel free to test it, and as always, keep the vibes!

[warmy]: https://crates.io/crates/warmy
[`Load::reload`]: https://docs.rs/warmy/0.5.2/warmy/trait.Load.html#method.reload
[changelog]: https://github.com/phaazon/warmy/blob/master/CHANGELOG.md#060
