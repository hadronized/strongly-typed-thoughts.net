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
