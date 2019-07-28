Lately, I’ve been wanting to re-write demoscene-like applications. Not in the same mood and way as
I usually did, though. Instead, I want to build small things for people to play with. A bit like
small and easy to use audiovisual experiences (it can be seen as small video games for instance,
but focused on the artistic expression as some games do).

See, the type of application you need to write to make a demo is a bit special. Most programs out
there are often either _oneshot_ or _reactive_.

  - A CLI program, an encoder/decoder, a math equation solver or a script are often _oneshot_
    programs. You give them a list of inputs and parameters and they do something based on their
    inputs. They’re are most of the time non-interactive.
  - Programs such as GUIs, editors, video games, simulations etc. are often _reactive_, meaning that
    they mostly sleep until an event happen (a key press, mouse movement, etc.). For some, sleeping
    is reduced because they have a lot to do (handling network connections, performing I/O
    operations, streaming audio, rendering frames to the screen, etc.).

That binary split is not absolute, of course. Some programs don’t belong to any of the two prevous
sections (think of a driver, a firmware, etc.). For the purpose of this article, though, it will be
enough.

At some extent, we could state that a demoscene production, a video game or a simulation are also
_reactive_ programs. They mostly react to time as well as user interaction, network events, etc..
About time, you could imagine dividing time passing with very small quanta and picture time as a
virtual clock that _ticks_ such quanta. Every time a new quantum of time is emitted, the whole
simulation is notified and reacts. However, representing that problem this way is not necessarily
the best idea. See, a function of time is often a continuous function and then, it has an output
value for any value of its input that lies in its domain. That property is interesting for us as we
can get a value at any time with an arbitrary precision (the boundary being the precision at which
we can represent a number on a computer).

The thing is, the kind of program we want generates its own inputs based on, mostly, the speed at
which the hardware it’s running on is able to render a complete frame. The faster the more accurate
we sample from that continuous function. That is actually quite logical: more FPS means, literally,
more images to sample. The difference between two images will get less and less noticeable as the
number of FPS rises. That gives you smooth images.

The “challenge” here is to write code to schedule those images. Instead of taking a parameter like
the time on the command-line and rendering the corresponding image, we will generate a stream of
images and will do different things at different times. Especially in demoscene productions, we want
to synchronize what’s on the screen with what’s playing on the audio device.

# The overall idea

From my point of view, we need at least two mechanisms of synchronization:

  - A *high-level* synchronization mechanism, to state how globally the application will behave.
    I also like to see that kind of mechanism as a _discrete space_ problem. You don’t have values
    to interpolate or sample from but only several pure values to “jump” from one to another every
    time the simulation time passes a given point in time. This kind of synchronization will state
    things like:
    - When the application starts, display this scene and let it play for _3 seconds_.
    - Then after _3 seconds_, switch to this other scene for _5 seconds_.
    - After _8 seconds_, draw this nice little cube and make it crash into a plane for about _10
      seconds_. Repeat that scene once more.
    - End the application.
  - A *low-level* synchronization system. That would be used for a movement, a color change, a
    camera change, etc. This kind of synchronization is, to me, a _continuous space_ problem. It
    can be solved by sampling from a curve, for instance.

Both those problems are solved by two crates I wrote lately. Respectively, [awoo] and [splines].
This blog post is about [awoo]. [splines] already has its own dedicated articles
[here](https://phaazon.net/blog/splines-introduction) and
[here](https://phaazon.net/blog/splines-1.0.0-rc.1). Nevertheless, I will make another blog article
about it because I have new ideas I will add to the crate to enrich the [splines] experience.

# [awoo] and the if / else if problem

Taking on the example of the *high-level* synchronization described above, one can write quickly the
following naive yet working snippet:

```rust
let mut time_ms: f32 = 0.;

loop {
  if time_ms <= 5. {
    // render scene 1
  } else if time_ms <= 8. {
    // render scene 2
  } else if time_ms <= 20. {
    // render scene 1 again
  } else if time_ms <= 25. {
    // render scene 3
  } else
    break; // quit
  }

  time_ms += 0.01; // 100 FPS
}
```

That code is typical in demoscene production when we have to rush or even if we have a few scenes
to write. However, it has several problems:

  - It’s not really elegant. All those `if`s seem like naive and not necessary code.
  - The more time passes, the more conditions and branching we will do. What it means is that even
    if it’s not noticeable (testing floating-point numbers like that is really fast so it won’t
    change much), it’s easy to get that the second scene requires two tests in order to be rendered
    while scene 3 requires four and scene 1 requires _either_ one or three!
  - We notice that, every time a condition evaluates to `true`, all the previous branches can be
    completely discarded — we don’t have to test them anymore! — because time will never go
    backwards in a simulation (that is a strong assumption and it’s not true if you’re debugging,
    but for a release application, it is).

So, how can we do better? The idea is actually pretty simple. We want a very simple form a
[finite-state machine]. In our case, the _states_ are just what’s inside our `if`s; the initial
state is the first scene being rendered and the transitions are a predicate on the current time.
Straightforward, right?

The idea of [awoo] is exactly that: allowing you to write the previous code like this:

```rust
let windows = vec![
  Window::new(0., 5.).map(|_| println!("hey, it’s scene 1!")),
  Window::new(5., 8.).map(|_| println!("hey, it’s scene 2!")),
  Window::new(8., 20.).map(|_| println!("hey, it’s scene 1 again!")),
  Window::new(20., 25.).map(|_| println!("hey, it’s scene 3!")),
];
let mut scheduler = SequentialScheduler::new(
  SimpleF32TimeGenerator::new(0., 0.01),
  windows
);

scheduler.schedule();
```

The code is now declarative and easier to read. Internally, the `SequentialScheduler` used here
will make a single test to know which code it has to run. The implementation is not the typical
implementation you would find for a FSM ([finite-state machine]), which uses a graph, but it’s akin.

You might be wondering why we do that `map` stuff instead of creating a `Window` directly with the
actions. The answer is simple: a `Window` doesn’t hold any actions. That allows for creating windows
via JSON, for instance, without having to deal with closures (I have no idea how that would even be
possible with JSON). The idea is then to _zip_ your windows to your actions by using a _hashmap_,
for instance. This following snippet showcases exactly that
(fully available [here](https://github.com/phaazon/awoo/blob/master/examples/json-driven.rs)):

```rust
const WINDOWS: &str = r#"
{
  "a": {
    "start": 0,
    "end":   3
  },
  "b": {
    "start": 3,
    "end":  10
  }
}"#;

let windows: HashMap<String, Window<f32>> = from_str(WINDOWS).expect("cannot deserialize windows");
let a = windows.get("a").unwrap().map(|t| println!("in a: {}", t));
let b = windows.get("b").unwrap().map(|t| println!("in b: {}", t));
}
```

What gets interesting is that you can write your own time generator to manipulate the simulation in
other ways — and you can also use different schedulers regarding what you do with time. For
instance, you can imagine implementing a time generator that gets time from a HTTP request, a
keyboard, a network socket, etc. and then control your simulation with external stimuli.

What happens when the escape key is pressed and that you need to stop the simulation in order to
quit? Simple: you need an interruptible scheduler. [awoo] offers that as well in this form:

```rust
use std::sync::mpsc::channel;

let (sx, rx) = channel();
let mut scheduler = create_your_scheduler();

scheduler.interruptible_with(move |_| {
  // here, the closure’s argument is the time at which the scheduler is checking for interruptions
  if let Ok(_) = rx.try_recv() {
    Interrupt::Break
  } else {
    Interrupt::Continue
  }
});

scheduler.schedule();
```

Here, we use the spinning loop of the scheduler to check for interruptions in a straight-forward
way.

So far, I have to admit I haven’t digged the `async`, `await` and `Future` concepts in Rust too
much. For a single reason: discussions around those concepts have been heated and I will wait for an
official announcement of the feature. Schedulers, especially as simple as the ones in [awoo], don’t
necessarily requires such IO features but the interruptible feature might. To me, the current
implementation of interruptible schedulers in [awoo] is sufficient, especially for animation
purposes — I might even add that feature directly in [awoo] so that you don’t have to do it by hand.

# About the scope of the crate

Currently, the crate’s scope is very narrow — and I actually like that. A tight and small scope
implies a better visibility about what the crate must do and how it must do it. The crate is
currently simple and it might get more and more complex stuff as needs appear. As I always tell
other developers and engineers, I don’t like to overthink too much features I don’t even need.
Obviously, it’s important to keep planning possible future additions… But not too much. This is why
that crate’s scope, if augmented, will only and always revolve around the concept of scheduling
animation code. It’s currently an experimental crate and I’m trying to write demos with it, so we’ll
see what time thinks about it.

So that’s all for me for today. I hope you liked it. Keep the vibes!

[awoo]: https://crates.io/crates/awoo
[splines]: https://crates.io/crates/splines
[finite-state machine]: https://en.wikipedia.org/wiki/Finite-state_machine
