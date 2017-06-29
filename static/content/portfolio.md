<section class="container section content">
<h1 class="title">Portfolio</h1>
---

Here’s a non-comprehensive list of all personal or school projects I worked or
I’m working on. I chose to show you those projects because I believe they’re the most representative
part of my work on my spare time. Feel free to test and give me your feedback!

The portfolio is organized by:

- [demoscene](#demoscene)
- [community](#community)
- [contribution](#contributions)
- [school](#school)

If you want a more comprehensive list of what I do, feel free to visit my
[github page](https://github.com/phaazon).
</section>

<section class="section hero is-info content is-right">
<h1 class="title">Demoscene</h1>
<h2 class="subtitle is-small">Artsy, size-limited, mathematical, underground</h2>
</section>

<section class="container section content">
I’m a demomaker. Here’s my [pouët](http://www.pouet.net/user.php?who=93160) page.

I joined the demoscene world a few times ago, but my first release was [Lightning
Road To Liquid Radiator](#lightning-road-to-liquid-radiator), at **Outline 2013**
in the **combined PC intro/demo** compo.
</section>

<div class="container card">
<div class="card-content">
<div class="media">
<div class="media-left">
<figure class="image is-128x128">
![Image](/static/imgs/celeri_remoulade.png)
</figure>
</div>
<div class="media-content">
<div class="columns">
<div class="column">
<p class="title is-4">Céleri Rémoulade</p>
<p class="subtitle is-6">Evoke 2016</p>
</div>
<div class="column">
<p>PC Demo</p>
<p>Ranked 14th/18</p>
</div>
<div class="column">
<a class="icon" href="https://github.com/phaazon/celeri-remoulade"><i class="fa fa-github"></i></a>
<p>Demozoo</p>
<a class="icon" href="http://www.pouet.net/prod.php?which=67966"><i class="fa fa-globe"></i></a>
</div>
</div>
</div>
</div>
<div class="content">
Blablabla
</div>
</div>
</div>

#### Céleri Rémoulade

![](/static/imgs/celeri_remoulade.png)

My third demoscene production – and first PC demo release. Released at Evoke 2016 (Köln, Germany),
and ranked 14th / 18.

It is a one-man production that I wrote entirely in **Rust** using my [luminance](https://github.com/phaazon/luminance-rs)
graphics framework, OpenAL and libvorbis.

[pouët](http://www.pouet.net/prod.php?which=67966)

[demozoo](https://demozoo.org/productions/161887)

[github](https://github.com/phaazon/celeri-remoulade)

#### luminance (Rust)

I learned [Rust](https://www.rust-lang.org) by March, 2016. Fallen in love with that language, which
is very similar to Haskell with the control over the memory and predictive performance as an extra
argument, I decided to jump in and migrate as fast as the very next day my Haskell luminance
framework. The migration took me some time, because Rust doesn’t directly match to Haskell. For
instance, Haskell has more powerful abstraction tools, forcing me to adapt the abstractions and
hack around for several hours with Rust’s constructs. As an example, Haskell’s *existential
quantification*, which doesn’t exist in Rust – I mapped it by trait object, btw.

The project starts to be mature, thanks to all the code I wrote in Haskell (36 releases), and that
I’m writing a demoscene production with it. So I’m testing luminance in real conditions, changing
the interface on the fly to make it more comfortable and benchmarking.

By the time of writing, it’s been 10 releases. I don’t blog a lot about it because I’m intensively
writing my demoscene release. But be sure, I will blog about it and write several articles on how to
dig in and how to use luminance.

[luminance](https://github.com/phaazon/luminance-rs)

[luminance-gl](https://github.com/phaazon/luminance-gl-rs)

### 2015

#### luminance (Haskell)

**luminance** is born from the ashes of **quaazar**. The aims of luminance are simple: it’s a
wrapper over graphics technologies (OpenGL) designed to be **stateless**, **type-safe** and
**simple**.

I think that project was the most *followed* of mine. I got feedback from people, a lot of talks
on *reddit* and *IRC*, I was asked help by people wanting to hear from the experience of *“making
something unsafe and stateful safe and stateless”*, because that’s exactly what luminance is all
about.

After more than 36 releases, I decided to migrate the framework into its
[Rust](https://www.rust-lang.org) version.

[luminance](https://github.com/phaazon/luminance)

[luminance-gl](https://github.com/phaazon/luminance-gl)

[luminance-samples](https://github.com/phaazon/luminance-samples)

#### quaazar

![](/static/imgs/quaazar.png)

**quaazar** was my first attempt at graphics programming serious stuff in Haskell for demoscene
purposes. After coming to the realization that I was building something way too much generic, I
decided to discontinue the project and extract a sub part of it (luminance).

[Github](https://github.com/phaazon/quaazar)

### 2014

#### Ionosphere

**Ionosphere** is my very first attempt at music making. Back from
[VIP2014](http://vip2014.popsyteam.org/), I decided to go wild, buy Renoise 3.0
and make a tiny but lovely song in 4 hours.

[This is what came out](https://soundcloud.com/phaazon/ionosphere).

### 2013

#### Heat Station

![](/static/imgs/heat_station.png)

**Heat Station** is my second *64k intro*, released at **Evoke 2013**. I wrote
it in *C++* with **skyoralis**, my demoscene 3D realtime engine. I released it
in Germany, at **Evoke 2013**. It was a test-release for my engine, and I had
to rush the *Windows* port, so take it as-is!

It ranked 4th/4, behind **farbrausch**, **Inque** and **Stroboholics**.

[pouët](http://www.pouet.net/prod.php?which=61729)

[youtube](http://www.youtube.com/watch?v=aU30N9YSpBY)

[github](https://github.com/phaazon/heatstation)

[download](https://github.com/phaazon/heatstation/archive/master.zip)

#### Lightning Road To Liquid Radiator

![](/static/imgs/lr2lr.png)

**Lightning Road To Liquid Radiator** is my **very first release as a
demoscener**. It’s a *GNU/Linux 64k intro* I released at **Outline 2013** in the
*PC combined demo/intro compo (demo, 4k, 64k)*; **May 9th-12th**. It ranked
4th/9. I’m pretty proud of it. The binary file weighs **42k** and has no data at
all.

If you plan to test it on your own *Linux* machine, you’ll find some hints in
the README file if you issue any trouble.

[pouët](http://pouet.net/prod.php?which=61355)

[youtube](http://www.youtube.com/watch?v=oUa2BvlDWYQ)

[github](https://github.com/phaazon/lr2lr)

[download](https://github.com/phaazon/lr2lr/archive/master.zip)

#### skyoralis

![](static/imgs/skyoralis.png)

My very own demoscene engine. It’s designed to help me write my demoscene
releases, especially *64k intro*. Written in *C++11*, *OpenGL* and *GLSL*.

Up to now, no public version is available. The first intro using
**skyoralis** was [Heat Station](#heat-station), but it actually used less
than 10% of its features. I need to make a bigger production before
releasing **skyoralis** in public.

I decided to close that project because I moved it into Haskell.

## Community

In that part of my portfolio, you’ll find personal project I made on my spare
time to solve people’s problems or to simply have fun.

You can find all the projects I made for a community on my [github](https://github.com/phaazon).

### 2015

#### luminance

An effort to bring Haskell a type-safe, strongly-typed and stateless graphics framework.
It’s built over [OpenGL](https://www.opengl.org).

Currently, no public release is available, but it will eventually land in hackage sooner or later.
A lot of people are interested in the effort I make with **luminance** – around 50 stargazers and
12 watchers on github. I also often write articles about **luminance** on my blog to keep people
informed.

[github](https://github.com/phaazon/luminance)

#### hid & msi-kb-backlit

Those two projects originate from my actual laptop, a MSI GS60 Ghost Pro, with backlit keyboard. The
colors are customizable and MSI has provided the users with a tool – SteelEngine – to customize
them. However, that tool doesn’t work at all under Linux nor Mac OSX. I decided to understand how
the protocol is used, and write my own tool. I also wrote the **hid** Haskell library to help me
write that tool, and both the packages are available as *BSD-3* softwares.

[hid github](https://github.com/phaazon/hid)

[hid hackagedb](http://hackage.haskell.org/package/hid)

[msi-kb-backlit](https://github.com/phaazon/msi-kb-backlit)

[msi-kb-backlit hackagedb](http://hackage.haskell.org/package/msi-kb-backlit)

#### smoothie

A Haskell package to make it easy to build splines. The project has received an interest from
Haskellers.

The name comes from the verb *“to smooth”*, which we can assimilate to smoothed curves.

[github](https://github.com/phaazon/smoothie)

[hackagedb](http://hackage.haskell.org/package/smoothie)

### 2014

#### al

I maintain an [OpenAL](http://www.openal.org) Haskell binding.

[github](https://github.com/phaazon/al)

[hackagedb](http://hackage.haskell.org/package/al)

#### phaazon.net

This website is a great example of fun I have on my spare time. Written in
pure *Haskell*, nothing else – yeah, a bit of *CSS*.

[github](https://github.com/phaazon/phaazon.net)

#### IRC tellbot

A tellbot over IRC. It’s used to leave a message to someone who’s off.

The latest version also integrates a remote control over IRC using a password.

[github](https://github.com/phaazon/tellbot)

#### Reactant

**reactant** is my very first attempt to *FRP* in *Haskell*. I designed the
library to be simple and not limited to `IO`. I learned *FRP* with
**reactive-banana**.

Up to now, I’m still working on it.

### 2013

#### monad-journal

A logger solution in Haskell. It exports a typeclass and a monad transformer for ease.

Because [ghc-mod](https://github.com/kazu-yamamoto/ghc-mod) uses it, **monad-journal** gets a lot
of downloads, and I’m pretty proud of it then!

[github](https://github.com/phaazon/monad-journal)

[hackage](http://hackage.haskell.org/package/monad-journal)

#### hsFModEx

The official *Haskell* *FModEx* binding. Up to now I maintain the raw binding.
It’s intended to be wraped up again (like *OpenGLRaw*). Up to now, the binding
works but not all functions are exported.

You can contribute by forking the
**Github** repository, editing the `Sound.FModEx.Raw.Core.Functions` file (you
can follow the syntax I use for the first functions in that file), then push
pull requests! I’ll accept them for sure!

[github](https://github.com/phaazon/hsFModEx)

[hackagedb](http://hackage.haskell.org/package/FModExRaw)

#### Leaf

leaf is a portfolio generator that aims to be really simple to use. My first
porfolio was written thanks to that tool! It enables you to write your portfolio
in *Markdown*, so it’s really convenient to use. You can find all directions to
get started on *Github*.

For those who’d like to contribute, you can push pull requests on the github
page.

[github](https://github.com/phaazon/leaf/)

[download](https://github.com/phaazon/leaf/tags)

#### Phraskell

**phraskell** is a *fractal viewer*. For now, it only supports *Mandelbrot*
set, but is planned to support more and more fractal types. It aims to be
user-friendly, and includes these features:

- fractal colorschemes (not yet) ;
- different kinds of fractal representations (standard, buddhabrot, etc.)
  (not yet) ;
- zoom with a zoom-frame in order to see what you zoomin ;
- specify the fractal progression at runtime (not yet)
- change the colors at runtime (not yet)
- screenshot (not yet)
- and others! (obviously not yet at all)

It’s written in *Haskell*, and is my first real *Haskell* program.

**01/07/13 update: I’ve saved that project for later since I’m very busy right
now.**


#### sdb

**sdb** stands for **S**imple **D** **B**uilder. It’s a *D builder* you can use
as well as you do with *rdmd*.

Actually, you don’t want to use **sdb**. Why, would you say? Well, first, even
if it’s quite okay for a simple builder, *rdmd* is likely far away better.
Furthermore, I haven’t updated nor even read any D lines from **sdb** for a
while. Finally, I don’t plan to do so in the future. You can consider *sdb*
*v0.9.5* the latest version. Forever (woah that’s so sad :( ).

[github](https://github.com/phaazon/sdb)

[download](https://github.com/phaazon/sdb/tags)

## Contributions

### Haskell

#### OpenGLRawgen

OpenGLRawgen is a nice C OpenGL Specification to Haskell generator. It reads from XML and outputs regular and convenient Haskell modules.

I submitted some patches, but nothing important though. I just help to fix issues.

[github](https://github.com/phaazon/OpenGLRaw)

### D

#### Derelict3 (aldacron)

Derelict3 is a collection of D binders over C dynamic librairies. I especially contributed to port some librairies for Mac OSX and Windows to make things more portable.

You can find aldacron’s Derelict3 [here](https://github.com/aldacron/Derelict3).

#### DerelictBASS (p0nce)

DerelictBASS is not part of the Derelict3 library, though it depends on its link system. It’s a nice D binder written by p0nce over the C sound library called *BASS.

You can find DerelictBASS here, and C BASS [here](https://github.com/p0nce/DerelictBASS).

## School

### 2011

#### Battlerobot

![](static/imgs/battlerobot.png)

This is a *Java* project I had to complete the semester before the last one at
”**IUT**”. Our teacher wrote a basic labyrinth and an interface for a player,
and we have to present an implementation that solves the labyrinth in local view
(the player only knows what rooms are next to him). Then all implementations
were put against each other like in a competition, and the best algorithm was
found regarding some traits like number of moves and so on.

The first part of the project was to write a correct implementation of the
player movements. The less moves the player does to find the exit point the more
points he’s given.

The second part of the project was to alter the implementation of our algorithm.
Indeed, since the teacher had added health points to players and traps that
decrease life, we had to implement a brand new way to run the labyrinth. The
less moves and more health points left at the exit point the more points he’s
given.

I won both the two parts. It was meant to because all folks used random
strategies like “the right handed way” or “I have no idea where I’m going to”,
while I implemented connected and oriented graphs, with local
pseudo-deconnections (isolation of known areas of the labyrinth to make it less
dense) and path finding based on complexe heuristics. It was a really nice
project!

The archive contains the java code you can execute, some labyrinths to test
(as .txt files), and two papers I have to write to explain all my choices.

[download](http://dimitri.sabadie.free.fr/Download/phaazon-battlerobot_labyrinthe.tar.gz)

#### Rewrite of “Game Of Life“

![](static/imgs/game_of_life.png)

**Game of Life** is a *cellular automaton* invented by **Jon Conway** in 1970.
At the “IUT”, we had to write our very own version, for the second semester. We
had to implement brand new features, such as being able to change the life
rules through a graphic interface.

We also had to write that project in *imperative C++*, with the **SFML** library
and use a *MV* (MVC without C) software architecture.

Note: I tried – for the fun – compiling it again. Turns out that the latest
version of **SFML** breaks retrocompatibility, then it’s not sure you are able
to compile it as well. Sorry then. If you really want to use it, it’s open
source, so write a patch on github, and I’ll accept the pull request! :)

[github](https://github.com/phaazon/iutbx1-ds/tree/master/gol)

[download](https://github.com/phaazon/iutbx1-ds/tags)

#### Rewrite of “Bejeweled”

![](static/imgs/bejeweled.png)

**Bejeweled** is a game with a 8x8 wired diamonds grid of different colors,
randomly set. Written in *imperative C++*, with a **Top10** ranking system,
multiplayer, and so on…

[github](https://github.com/phaazon/iutbx1-ds/tree/master/bejeweled)

[download](https://github.com/phaazon/iutbx1-ds/tags)

#### Rewrite of “Pong”

![](static/imgs/pong.png)

We don’t introduce this game anymore. Written in *imperative C++*. With fancy
effects. No just kidding. Written in one hour :D.

[github](https://github.com/phaazon/iutbx1-ds/tree/master/pong)

[download](https://github.com/phaazon/iutbx1-ds/tags)
