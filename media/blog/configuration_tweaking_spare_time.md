# Configuration tweaking and spare-time

Oh, this is a topic I’ve been wanting to talk about for a long time: what is important to a developer in terms of personal
configuration regarding the tools they use and how do they spend their spare time to do so. I’ll talk more specifically
about editors, of course.

## Configuring and scripting

_Configuring_ is a bit of an overloaded term these days. To me, people tend to confuse _scripting_ with
_configuration_, which is really unfortunate because both are required and the audience / target users are not the same.

_Scripting_ targets mainly developers. In the case of a tool you use like a text editor, a shell, a browser or a
window manager, scripting is done by the core contributors or plugin developers to provide and extend features.

_Configuration_, on the other side, targets end-users; it targets people who are going to use the system you provide
them with in order to do _something else_. For instance, people using IntelliJ IDEA or VS Code to develop open-source or
proprietary projects don’t have to know (pro tip: I really think they don’t actually care) how a plugin is made or how
it’s supposed to be set up.

The hard realization here is that _some people_ have understood that, and they have understood it for a while now. Look
at editors such as VS Code or Atom, for instance. The distinction between configuration and scripting is clear. Because
I’m an editor enthusiast, I have spent a lot of time in many. VS Code and Atom have their UX fundamentals done right:
they have a clear distinction between _configuration_ and _scripting_. A user doesn’t have to know CoffeeScript,
TypeScript or anything else to configure them. For some editors, they don’t even have to know JSON! Those editors
provide a native configuration layer, which is, to me, something that should be in any decent editor.

## The problem

Now let’s talk about my two main editors: Neovim and Emacs. Both have been around for a long time, even though Emacs is
a dinosaur, since its first release was in 1976 (!!!). It’s actually impressive how Emacs has been evolving ever since
to the incredible productivity platform it is today. Anyway, Neovim and Emacs. Both editors share the « old » way of
doing that kind of UX: they don’t split them. Scripting comes via an EDSL or dynamic language (Emacs-Lisp, VimL, Lua)
and allows you to interact with the public API of the editor. This is the « plugin scope. » However, the rest is really
just decoration and layers. There is no support for configuration (Emacs does have `M-x customize` though, which is a
bit akin to VS Code’s way of doing things, but _not completely_ since there are things you can’t do with `M-x
customize` properly). « Configuring » Emacs or Neovim means « calling functions / commands in a special script,
typically called `init.el`, `init.lua` or `init.vim` that change values of the editors and its plugins. » The difference
between setting a variable in Lua in Neovim and setting a plugin’s feature is… well, there is no difference. You do that
exactly the same way: you typically set a variable or you pass a Lua table to a `setup` function.

This lack of separation of concerns has always worried me, especially since I care a lot about UX design and because I’m
the both users: I’m a plugin maintainer (I’m the author and maintainer of [hop.nvim]) but also a user who wants to just
install something and be done with it, without spending half a weekend writing Lua code to make things work. I think
everybody who has work to do (for your job or open-source / spare-time projects) will say the same thing: spare-time is
a finite resource. I’m actually more on the “like to tweak” side rather than “let me code ffs.” But I do think that
_requiring people_ to spend hours and hours learning a language (be it Emacs-Lisp, VimL or even Lua) and tweaking their
configurations just to be able to use a plugin or setup LSP and Treesitter is a step in the wrong direction, especially
since we _know_ that other editors do split configuration and scripting and that they’ve been massively adopted and huge
successes, especially since we are living in 2021 with all the UX and UI knowledge built from the past decades.

So the worrying point to me is: why haven’t Neovim or Emacs people thought about reversing that method and adopting a
more modern take? The topic has been discussed many times and I have to admit that among all the « new features » that
you can see in Neovim, I’m surprised to see integration of features such as remote plugins or even Lua actually (while
they could have decided to stick around VimL for more years), and the very fundamentals are not done correctly. Of
course here, « correctly » is from my own perspective, I’m sure you will find people who prefer not to split
configuration and scripting, but I don’t and think this is an old and deprecated design. I’ve had discussions with a
bunch of Neovim contributors who told me that Neovim can already do « declarative configuration » by simply passing Lua
tables around. That’s still code, that’s still scripting, that’s not configuration. There is no difference between
passing a table representing a user configuration from a function taking an object and doing an opaque thing with it.
That’s not easily introspectable. That’s not easily maintainable and that still requires people to know how to pass Lua
tables around. This is just the wrong direction to me. Emacs is better in that sense as it has the concept of
introspectable configuration variables, allowing you to list them, fuzzy search them, provide ad-hoc customization means
via plugins consuming and setting those lists, etc.. People still must set them in an Emacs-Lisp script but they at least
can use a different concept to do so or even use the `M-x customize` UX. Not perfect, but better.

I also remember people telling me that configuring via scripting is better because they have more opportunities. For
instance, they have the opportunity to do branching, depending on the host system, the date and time, etc. That is just
a bad design in disguise, to me. More opportunities also means more opportunities to do something wrong. To introduce a
badly designed piece of code. To ask people to write code and then potentially introduce slow-downs (especially since
Neovim uses Lua, in which you can do _anything you want_, even make a gRPC call while loading your Neovim
configuration).

If you want logic, then you want a plugin that provides that logic, so that you can later configure it. Separating and
splitting both doesn’t mean preventing people from still « scripting their configurations. » It’s a matter of how things
should be done but configuration variables should be exposed by plugins (the way Emacs does it), and be changed via the
more regular user-facing method (like VS Code, Atom, IntelliJ, `M-x customize`, etc.).

I have been a power user of Vim and Neovim for 13 years now. I’ve been using Emacs (at the beginning, when I was
younger) and lately Doom Emacs has been a big change to me. But to me, both editors still feel old to me regarding their
UX choices. Not everyone who enjoys a good editor is willing to spend hours tweaking their configuration. I personally
want something like Emacs or Neovim, but with the VS Code UX. You want to install a plugin? Just install it and « enable
» it and done. You want to write a fuzzy finder plugin and be able to switch the method the user can use to fuzzy
search? Declare a configuration facet / setting, use it in your plugin and let the user set it in the configuration view
of the editor. On that topic, I had started a project called [poetry.nvim], which was my “Eh, I’m a bit sick of having
to write all that Lua just to configure basic things, so I’m just going to create a configuration layer and let users
set everything in JSON”, but it’s not complete and I faced issues with the package managers (Poetry handles packages /
plugins for you by passing the JSON description to package manager backends) and the interface I picked (the _de facto_
`setup` Lua function exposed by the new generation of plugins) doesn’t allow introspection — i.e. you can’t get a list
of configuration options for a plugin. That would be a feature I would happily push to Poetry but it would require
people to implement that interface, and because this is such a core / common thing, I think it should be done by Neovim
core and not a community plugin.

This is the exact same discussion as with [telescope.nvim], which is a pure Lua implementation of fuzzy finding that is
massively adopted by the community as the new “user listing interface”, and I think it’s wrong, because that’s such a
common thing to do that the **interface should be in Neovim core**, and I’m talking about the interface only. Yes,
Neovim already has an `input()` function to ask the user to type something in the command line, but it should also have
a `pick_list(list_of_things)` function so that plugins can depend on that function, available everywhere, and let
backend implementations decide how to present the list, sort it, etc. This is just a basic and 101 of design: try to
expose _weak abstractions_ first. From a plugin perspective, you don’t care about how the list is going to be displayed,
sorted, filtered, etc. You only care about passing a list of options and what the user will pick. The abstraction should
then be a simple function taking a list of things (or a generator / coroutine so that you can lazily build that list,
which is super important when listing tons of stuff like files on the filesystem, for instance), and that’s it. Such a
feature doesn’t exist in Neovim and it saddens me a bit because it would allow plugin maintainers to reduce their
dependency graph and people to pick any fuzzy finder plugin they want. It would be a better overall design. You like
[telescope.nvim]? Then just set in your configuration that the `pick_list` backend is telescope. You prefer fzf? Set it
to fzf’s. You want to write your own? Go! Etc.

Another point I think would be beneficial would be to introduce SemVer but…

## Eh…

I completely drifted away from the initial configuration and spare-time topic, but it’s my blog! I do think people like
me, text editor enthusiasts, would have a lot to say about all that. I let some time pass regarding the direction of
Neovim because it’s an editor I’ve been hacking with since its fork from Vim, and been using Vim for more than a decade
before that. I’m not satisfied with the direction the editor is going to, especially when I see what others are doing.
Emacs, VS Code, Atom, IntelliJ, etc. When people say “I prefer using Vim / Neovim for the keybindings”, I think this is
such a hard misconception nowadays. If Vim / Neovim is just about the keybindings and modal editing, then you can
clearly drop both and use something like Emacs with `evil-mode`, which is the best Vim implementation after Vim itself.
It changes the way you look at software, because I now know that what I like about Vim and Neovim is not the software
itself, it’s the modal editing, that you can replicate in any editor allowing scripting. VS Code does it. Emacs does it.
Atom does it. IntelliJ does it. Some of them are not doing it perfectly (hence why I think `evil-mode` is the best
implementation), but that’s just a matter of fixing those other plugins. What it means is that modal editing and
keybindings **don’t define an editor**. So what’s left then with Vim and Neovim? The runtime performances – whic are
great – and design choices, for which I think the design is old and needs to be rethought.

Keep the vibes!

[hop.nvim]: https://github.com/phaazon/hop.nvim
[poetry.nvim]: https://github.com/phaazon/poetry.nvim
[telescope.nvim]: https://github.com/nvim-telescope/telescope.nvim
