I’ve been thinking of writing such an article for a while. A few weeks ago, I got contacted by
people who wanted to know more about my experience with
[luminance](https://github.com/phaazon/luminance) so that they can have more hindsight about their
own APIs and products.

I came to the realization that I could write a blog entry to discuss designs decisions and, at some
extent, what a good design entails. Keep in mind it’s only personal thoughts and that I won’t talk
for someone else.

# Elegancy

I love mathematics because they’re elegant. Elegancy implies several traits, among *simplicity*,
*flexibility* and *transparency*. They solve problems with very nice abstractions. In
mathematics, we have a concept that is – astonishingly – not very spread and barely known outside
of math geeks circles: *free* objects.

The concept of *free* is a bit overwhelming at first, because people are used to put labels and
examples on everything. For instance, if I say that an object is *free*, you might already have
associated some kind of *lock* to that object, so that you can get why it’s *free*. But we’re
mistaken. We don’t need *locks* to define what *free* implies. In mathematic, a *free* object is an
object that can’t be defined in terms of others. It’s a bit like a *core* object. It’s *free*
because it can be there, no matter what other objects are around. It has no dependency, it doesn’t
require no other interaction. You can also say that such an object is *free* of extra features that
wouldn’t be linked to its nature.

This *free* property is a very interesting property in mathematics, because it’s surprisingly
simple! We can leverage that mathematic abstraction to software design. I like keeping my softwares
as much *free* as possible. That is – with a more human language to say it – constraining them to
keep low responsibilities about what they’re doing.

# Responsibility domains

The important thing to keep in mind is that you should, at first, define what the responsibility
domain is all about. Let’s say you’d like to create a library to implement audio effects, like the
[Doppler effect](https://en.wikipedia.org/wiki/Doppler_effect) – that effect actually exists for
any kind of wave, but it’s interesting to synthetize it for a sound-related application. If you end
up writing functions or routines to play sound or to load audio samples, you’re already doing it
wrong! You’d have violated your reponsibility domain, which is, *“audio effects”*. Unfortunately,
**a lot** of libraries do that. Adding extra stuff – and sometimes, worse; relying on them!

A lot of people tend to disagree with that – or they just *ignore* / *don’t know*. There’re plenty
of examples of libraries and softwares that can do everything and nothing. For instance, take
[Qt](http://www.qt.io/) – pronounce *cute* or *cutie*. At first, *Qt* is a library and an API to
build up *GUIs* – Graphical User Interfaces – and handle windows, events and so on. Let’s have a
look at the documentation of modules, [here](http://doc.qt.io/qt-5/qtmodules.html).

You can see how the responsibility domain is **huge**! GUI, radio, audio, video, camera, network,
database, printing, concurrency and multithreading… *Qt* isn’t a library anymore; it’s a whole new
language!

People tend to like that. *“Yeah, I just have to use Qt, and I can do everything!”*. Well, that’s a
point. But you can also think it another way. Qt is a very massive “library” you’ll spend hours
reading the documentation and will use a lot of different classes / functions from different
aspects. That doesn’t compose at all. What happens when you want to – or when you don’t have the
choice? – use something else? For instance, if you want to use a smaller–but–dedicated threading
library? What happens if you want to use a database service you wrote or that you know it’s great?
Do you wipeout your Qt use? Do you… try to make both work in harmony? If so, do you have to write a
lot of boilerplate code? Do you forget about those technologies and fallback on Qt? Do the concepts
map to each others?

The problem with massive libraries is the tight bound it creates between the libraries and the
developers. It’s very hard with such libraries to say that you can use it whenever you want because
you perfectly know them. You could even just need a few things from it; like, the *SQL* part. You’ll
then have to install a lot of code you’ll perhaps use 10% of.

# KISS

I love how the *free* objects from mathematics can be leveraged to build simpler libraries here. The
good part about *free* objects is the fact that they don’t have any extra features embedded. That’s
very cool, because thanks to that, you can reason in terms of such objects *as-is*. For instance,
[OpenAL](http://www.openal.org) is a very *free* audio library. Its responsibility domain is to be
able to play sound and apply simple effects on them – raw and primary effects. You won’t find
anything to load music from files nor samples. And that’s very nice, because the API is **small**,
**simple** and **straight-forward**.

Those adjectives are the base of the [KISS principle](https://en.wikipedia.org/wiki/KISS_principle).
The ideas behind *KISS* are simple: keep it simple and stupid. Keep it simple, because the simpler
the better. A too complex architecture is bloated and ends up unmaintainable. Simplicity implies
elegancy and then, flexibility and composability.

That’s why I think a good architecture is a small – in terms of responsibility – and simple one. If
you need complexity, that’s because your responsibility domain is already a bit more complex than
the common ones. And even though the design is complex for someone outside of the domain, for the
domain itself, it should stay simple and as most straight-forward as possible.

# API

I think a good API design is to pick a domain, and stick to it. Whatever extra features you won’t
provide, you’ll be able to create other libraries to add those features. Because those features will
also be *free*, they will be useful in other projects that you don’t even have any idea they exist!
That’s a very cool aspect of *free* objects!

There’s also a case in which you have to make sacrifices – and crucial choices. For instance,
event-driven programming can be implemented via several techniques. A popular one in the functional
programming world nowadays is [FRP](https://wiki.haskell.org/Functional_Reactive_Programming). Such
a library is an *architectural codebase*. If you end up adding *FRP*-related code lines in your
networking-oriented library, you might be doing it wrong. Because, eh, what if I just want to use
imperative event-driven idioms, like [observers](https://en.wikipedia.org/wiki/Observer_pattern)?
You shouldn’t integrate such architectural design choices in specific libraries. Keep them *free*,
so that everyone can quickly learn them, and enjoy them!

I like to see good-designed libraries as a set of very powerful, tiny tools I can compose and move
around freely. If a tool gets broken or if it has wrong performances, I can switch to a new one or
write my very own. Achieving such a flexibility without following the *KISS principle* is harder or
may be impossible to reach.

So, in my opinion, we should keep things simple and stupid. They’re simpler to reason about, they
compose and scale greatly and they of course are easier to maintain. Compose them with architectural
or whatever designs in the actual final executable project. Don’t make premature important choices!
