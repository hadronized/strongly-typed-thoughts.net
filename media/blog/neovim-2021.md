For the past year, I have been moving back and forth between [Neovim], my main, daily editor, and [Doom Emacs], an
[Emacs] configuration distribution focusing on bringing a super consistent, powerful and overall top-notch user
experience for [Emacs]. This article is a direct follow-up to the
[previous one](https://phaazon.net/blog/editors-in-2020) from fall 2020.

In this blog entry, I want to share something a bit different: instead of presenting my experiences with all those
editors, I want to take some hindsight and analyze a bit more the technology and what it seriously means in terms of
features and productivity. Obviously, I will reference other editors I have used a lot (I use [IntelliJ] a lot at work,
and I am used to [VS Code] and [Atom] as well).

# Current status as of March 2021: I am lost

Yeah, you read that title right. I am lost. I have been a Vim / Neovim user for something like 12 years now. I am used to
the ecosystem, I have installed hundreds of plugins in the past decade, tried so many things, discovered pearls that I
still use today (hello [context.vim]), I have been using a workflow based on [tmux], Neovim and my shell that has proven
very powerful and yet to be fully replaced. But — and I think it is important to make that note — I did not start coding
with Vim. Today, I am 29. I started coding when I was 11, using a tool called [Code::Blocks]. Rapidly, I switched to
Emacs – by the time, Doom Emacs did not even exist, and spent several years using it. Later, when I was between 16
and 17, I switched to Vim — I guess the modal idea got me intrigued and I felt in love with it. In 2014, I was told that
Vim was getting forked for the reasons we all know and I decided to switch to the fork, called Neovim.

Since then, I have been sticking to Neovim but I do not partake in those “editor wars”. “VS Code sucks”, “how do I even
exit Vim?”, “Emacs is bloated!”. I have my personal opinions on the technologies (i.e. I dislike very much the front-end
ecosystem / electron, even though I think the front-end idea — i.e. running code in a browser — is an interesting idea;
I just think the current technology for doing so is really bad), so obviously I will not pick something such as Atom /
VS Code, but I cannot say they suck. I have tried them. I see why people like them and it would be very dishonest to say
those editors suck. They really do not. Same thing applies to the products by JetBrains. In the past year, I did not
understand why some people would pay money for an editor. I have to use IntelliJ at work and even though I would not pay
for an editor, I start to grasp why people would actually want to pay for one. Not all people have the patience and time
to tweak an editor to their taste, such as with Vim / Neovim / Emacs. For those people, editors such as VS Code,
IntelliJ, Atom and others are really just superior.

Just take [LSP]. Configuring LSP in VS Code is a click on an install button, depending on your language. That is all.
You get all the good defaults and behaviors and you can already start enjoying coding with LSP. Doing the same in an
editor like Neovim is a very different experience. I am not saying it is bad; **it is different**. If you are, like me,
someone who enjoys spending time tweaking their tools, then you should not care too much. I have a colleague at work who
gave me an argument that I find very interesting: he basically wants an editor he does not have to configure, so that we
knows how to use it everywhere, on any machine of any colleague. That is not a point I would make, but for someone who
cares about that topic, it is a solid argument in favor of tools such as IntelliJ or VS Code.

In the previous company I was working for, I worked with [\@gagbo], who is an active Doom Emacs contributor, and I was
surprised by the editor he was using on his laptop. He mentioned Doom and on the night home, I had a look at it, and I
was really blown away. Doom Emacs (Doom for short) was a revelation to me, because it is using emacs as a productivy
platform more than a simple editor. [\@hlissner], Doom’s author, and the Doom community, have been gathering the best of
emacs and vim into a single configuration distribution. The result is so interesting and so powerful that I have been
doing back and forth between Doom and Neovim. More on that below.

Lately, I have been more present in the Neovim community, contributing mostly via a plugin that I have been making:
[hop.nvim]. That plugin is a complete rewrite from scratch of [EasyMotion] (I have not even looked at how they do it on
their side) I made by using the Neovim Lua API directly. Obviously, it provides a much better experience than
EasyMotion **in Neovim**, simply because it uses features such as _virtual text_ and _extended marks_ and doesn not
touch the buffer, allowing to benefit from Neovim-0.5 crunchy features, such as the native LSP client and treesitter
parsers. I also made a bunch of PRs to Neovim core, reading a bit on the C code, etc.

All that to say that… given the experience that I have with Neovim and Doom… I have no idea what to think about
“editing” anymore. Neovim is growing and lots of interesting ideas are landing / will land in the next release. I do not
really know whether people will stick around Hop, but I think it has found its place — at least, I spotted a missing
piece and provided it. I am not a fan of Lua but I have to admit that it attracts more people doing some awesome things.
Among the things that I think are going into the right direction:

- The native LSP client.
- The native treesitter implementation.
- [Nvim Tree], a file browser written in Lua. Similar to netrw but better and much (much) faster.
- [Telescope], a fuzzy finder, a bit similar to [Ivy] / [Helm] from the emacs realm. I have issues with it right now
  because I am used to [fzf-vim] and the all the current sorting / picking algorithms are not up to the user experience
  of fzf to me. When I compare it to [Ivy], it is roughly the same: [Ivy]’s algorithm is just better to me (it has the
  same refinement search mechanism as fzf’s). I think it will change in the near future as algorithms in Telescope are
  improved and support is added for (I hope!) fzf, but for now, the bad performances of telescope, the choice of making
  it pretty before focusing on the algorithms first makes it not really usable on some code bases.
- [Neogit], an implementation of the best Git client ever, [magit], also from the emacs realm. I think it will take them
  months before it reaches the same maturity and power as magit, but I think it is cruising great.
- [Octo], a GitHub-based workflow plugin allowing to list, create, open, add, remove, etc. lots of GitHub objects
  directly from within a Neovim buffer, streaming changes on buffer exit / save. Similar, again, to magit’s [Forge],
  from the emacs world, even if Forge is a bit more powerful as it abstracts away the concept of “remote”. However, the
  edit workflow of Octo seems a bit more stable right now, so kudos!
- Probably a ton of other plugins I forgot to mention.

# So what is wrong?

My experience with Doom Emacs really changed the way I look at software, and more specifically at “productivity
tools.” See, my current workflow is based on tmux, Neovim and my shell, which is zsh (vanilla, I completely ditched
[Oh My Zsh] as I find it useless and I uninstalled [starship.rs] a few weeks ago because I realized I hated the package
managers version annotation on my prompt, that I actually never needed). For people wondering, we have spent so much
time trying to install cool things that we forgot how powerful raw and vanilla zsh is. This is my current prompt:

![](https://phaazon.net/media/uploads/current_zsh.png)

At work, I need a bit more information (i.e. Kubernetes (k8s) namespace and context / cluster I am currently connected
to and AWS), but as those information are not path-dependent, I put them in my tmux statusline instead.

The thing is… I just realized by using Doom Emacs that I do not need all of these anymore. Doom Emacs provides an
environment that is very similar to my tmux + neovim + shell combo. It provides even more, depending on the packages you
install. And the thing is: for each tool, emacs have more opportunities, because it is not limited by a terminal
implementation (i.e. cell-grid / line based application). So for instance, you can have a k8s / AWS / docker client
inside Emacs that is likely to be much much better than the one you will use in CLI (I honestly dislike `kubectl` a lot,
it has so many switches and grepping through them by piping with `rg` or using some switches such as `-l` feels weird).
You can easily see a buffer containing the list of pods / containers / whatever, that you should be able to work with
the tool you already use (i.e. [Ivy] for instance), interact dynamically with (instead of having to type one line after
another to make effects). It is even more true when you consider the actual shell: you do not need zsh anymore when
using Emacs, because it has its [eshell] implementation that is both super weird and super nice. You want to script
something for your shell? Just write ELisp. You do not have to use that horrible bash script language, and you will
benefit from the whole ELisp ecosystem. Completion for commands in eshell is basically the same as completing a regular
buffer, which is weird at first but actually makes a lot of sense. And you have dozens of other examples. Org-mode is
one of the best plugin of Emacs, allowing to take notes, make wikis, handle tasks and calendars, in a way that feels
very seamless with the rest of the Emacs ecosystem. I know some people will tell me to use another tool (I do, my
current tool for that is [toodoux], a tool I made, similar to [task-warrior]). And of course, [magit], which is by far
my favorite Emacs plugin. As others say it, it is hard to explain why magit is so much better than the git CLI or
VS Code’s integration or basically any other editor. It is just made very well, with pretty much anything actionable
from the UI, with amazing workflows and even actions you do not have easily with the CLI version.

# What is an editor in 2021, anymore?

Today, when I look at Doom, at Neovim, at what people are trying to do in Lua with Neovim, I feel like I need to stop
trying to implement / follow what others are doing and think a bit about whether we are doing the right thing. Neovim,
currently, is going towards a very exciting direction with all the effort regarding Lua, LSP, treesitter and community
plugins. But I think the problem is bigger than Neovim. And I think I know that because of how **I can use emacs with
Doom**. I think I come back to Doom so often because they understand well what it means to create a productivity
platform more than an editor. It does not mean the editor is any less good, _au contraire_. In Doom, there is a default
package that gets installed if you ask for the project module. That package is [projectile], a wonderful package that
provides with you project management from within emacs. If you use emacs in daemon mode, it means that you can get the
same workflow as with tmux, Neovim and a shell, but with a native implementation of this project / workspace workflow,
in a much much more powerful way (because it will obviously plug in things like [Ivy], allowing you to do fuzzy
searching of projects, etc.). The overall experience by having a consistent platform is something I had completely
ignored for years and now that Doom hit me, it hit me hard. I do not know _any other platform_ like emacs, besides
composable environments such as tmux + Neovim + shell. Yes, you have VS Code / Atom / IntelliJ / etc. that will have
tons of plugins to get you terminals and other kinds of integrations, but for having used them, it is clearly not as
powerful as what Doom does — and it is most of the time based on electron; big no. To sum up, Doom is a bit my
VS Code: it provides this amazing productivity platform, but it is fully native, modal-centric and highly composable /
customizable.

Composability is really important to me, but I have to face reality: there is _no way_ to make a workflow such as tmux +
Neovim as snappy and fast as [projectile]. [tmux] basically reimplements a terminal inside a terminal, which has a lot
of implications (copy-pasting with the mouse in tmux is a challenge even Chuck Norris does not want to take). And once
you turn the computer off, tmux loses everything. You have plugins, such as [tmux-resurrect], that will allow to save
and restore the tmux layout (sessions / windows / panes), but you will obviously lose the contents of each panes. With
[projectile], because it is a native emacs thing, you will not lose anything.

This whole idea has been obsessing me for months. The concept of a “productivity platform.” People laugh at emacs as
being bloated but… it basically implements their shell, tmux and editor in a more elegant way — and more. Why would one
laugh at that? Especially when it allows more interesting kinds of interaction.

So I have been wondering: what is a productivity platform, today, in 2021? Is it this workflow I have been using since
I am 16, with tmux and Vim / Neovim? A friend on IRC told me lately that if I have been using this workflow (as others
do) for so long… it has proven being good so why would I want to replace it? Even though this is a valid point, I do
thing this is missing the point: ask some workers to dig a hole for planting trees, they will start digging the ground
with their bare hands. Later, give them a shovel and they will accept it as a better tool and will enjoy it. But does it
mean it is the best tool for planting trees? What if a better tool existed? This is basically the reason why I never
accept something as an ultimate solution: we can always enhance our tools. And having seen what Doom Emacs do, I do
think that I have found new ideas. Not necessarily a better tool, but at least, I think Neovim needs to evolve.

When I think about Neovim, I try to focus on what makes it this tool I love: modal centric, snappy / performance,
integrates well with tmux, and that is pretty much all. The integration part with tmux is needed because I have to use
the TUI, which is also something that I start not liking anymore. I know since Doom that we can get rid of TUIs and make
great GUIs. So what I think I want to go to with Neovim from now on is this:

- Contribute to a GUI or even make my own. I have glanced at all the current solutions and none are really exciting to
  me. People have basically been making window decorations around TUI buffers, which I truly do not like. Some
  exceptions that I might invest time in:
  - [goneovim](https://github.com/akiyosi/goneovim), which seems to have the more advanced “GUI” externalized features.
  - [uivonim](https://github.com/smolck/uivonim), but I will not contribute nor use it because, heck, electron.
- Contribute to [neovim-ui](https://github.com/mjlbach/neovim-ui) and see where I can help with abstractions.

Something around one year ago, I chatted with core contributors, telling them that something that puzzles me a bit with
the current Neovim situation is that there is no unified / consistent UI experience. Most plugins will write their own
UI thing, obviously often based on TUI hacks, such as using characters as `─`, `╮`, `├`, etc. The current situation with
[plenary.nvim](https://github.com/nvim-lua/plenary.nvim) is another interesting topic, because I think it is not the
answer to my problem. Yes, it will provide an abstraction for people needing to create bordered windows / pop-ups but…
the GUI really does not care about that. Technically, Neovim already makes the distinction between “buffers” and
“pop-ups” and “floats”, so GUIs are free to render them the way they want, but… what if developers start using APIs,
such as plenary, where they pass border arguments? How does that make sense? If you are writing a plugin that needs to
list things and requires a fuzzy interaction with the user, you should not depend on something that expects anything
from the UI implementators. And for this, I think Neovim is lacking abstractions. Obviously, we can discuss that kind of
problems for other topics:

- Displaying messages.
- Displaying notifications (I wish Neovim used the D-Bus protocol or similar on Linux for instance).
- Fully customizable sign columns and “franges.”
- Etc. etc.

I guess some of those items are already addressed, but I have yet to see a Neovim GUI going fully “native without TUI
aspects.” Maybe it already exists and I need to search again. I want to invest time in things like goneovim and some
other implementations, and I really do want to make my own (maybe with Gtk or even using my own 2D/3D graphics Rust
stack).

# Next obsession: CLI, TUI and GUI

> CLI stands for Command Line Interface; TUI for Terminal User Interface and GUI for Graphics User Interface.

Among all the things that Doom changed in my mind, there is this concept of TUI. I have been a very big user (power
user?) of the CLI. Pretty much anything that I do on a computer is via the CLI. For more advanced usage, I tend to use
a TUI. I only use GUIs if I have to, such as [firefox], [blender] or a video game. However, guess what: Doom hit hard
here too, because I am not using the TUI version: I am using the GUI one. And do not get it twisted, I am not using my
mouse, and this is a very important and interesting point about CLI / TUI / GUI.

When talking about this CLI / TUI vs. GUI thing around me, people tend to agree, weirdly, on some misconceptions:

- CLIs and TUIs, because they are mostly implemented as terminal applications, would not rely (for most) on the mouse and
  then implement super good user experiences (UX): good shortcuts, good navigation through the application, easy
  completion, great composability, etc. The UI, on the other side, would be rather poorer, because of the limitations of
  the terminals: no images, only icons, which most of the time require patched fonts to render icons; weird hacks based
  on unicode characters to mimic UIs like borders, splits, buttons, inputs, etc.
- GUIs would rely more on the mouse and then focus less on the UX, making them harder to use for people on the keyboard,
  with shortcuts most of the time not that good, preventing fluid and seamless navigation on the keyboard; poor
  composability with other applications, etc. However, they would have much better UI, because of vector graphics images,
  proper UI elements such as real inputs, buttons, border, splits, different font families / sizes, etc.

These misconceptions are interesting because, as misconceptions, they are not real. You can find some terrible CLIs
(i.e. `npm` is such a bad CLI — I had a big issue at work a few years ago when running not on purpose a command such
as `npm i --user`, thinking it would install it in user space while it just completely ignored the unknown `--user`
flag and installed the package globally), and you can find terrible GUIs too. However, and this is where I want to
focus, GUIs are not necessarily mouse-focused, and since we can implement nice UX in TUI… we should be able to do the
same in GUIs, right?

Aaaaaaaand… enter Doom, again. I am using the GUI version of emacs — on Archlinux, I am using this
[patched version](https://aur.archlinux.org/packages/emacs-native-comp-git) which is a basically [gccemacs], an emacs
that recompiles all the Lisp as C! It obviously can accept mouse inputs so that you can click on things if you want to
but… the most important still applies: you can use it exactly as you would use it in a terminal. However, because it is
a GUI, you get access to much more possibilities:

- Proper “pixel scrolling.”
- Use several fonts, of different sizes, for things like Markdowns, note taking, etc.
- Proper pop-up windows.
- Pixel-perfect annotations, marks, hints and signs in your buffer.
- Images, rasterized or vector graphics.
- One of the most important part to me: proper UI elements! No more hacks!
- Even though it is not implemented yet, since we can have images, and especially vector graphics, we should be able to
  render git branches with pretty links and not the `/|\*` ASCII art mimics we see in all CLIs / TUIs.
- Etc. etc.

Today, I do think that the right direction is to allow developers to build platforms around the concept of “graphics
toolkits”, and not “terminal applications.” Even for CLIs. Think about `git branch`. My current git branch workflow, in
CLI, does not use `git branch` only: I have an alias for each Git commands that require a branch. For instance, if I want
to switch to another branch, this is what my `gs` alias looks like when invoked:

![](https://phaazon.net/media/uploads/git_switch.png)

This is basically a fuzzy-finder version of `git switch`, using [fzf] – if you want that script, have a look at
[these ones](https://github.com/phaazon/config/tree/master/git/scripts). The same feature is doable with Doom / Neovim
by using the right plugin, and I have to admit that I prefer doing that in magit as it feels more “well integrated.” Of
course, you will always want to have a way to compose applications, and for that, CLIs are great. Something such as
`kubectl get pods -l service=my-service | rg whatever`. However, I am trying to challenge my own conception of
“program composability” because I do think we can implement this _list k8s pods -> filter them_ inside an UI such as the
ones in Doom Emacs, without having to write a specific tool. And we can actually do it in CLI / TUI as I just showed
with my `gs` alias, so… why not providing that kind of power natively on the platform?

# Final obsession: configuration ≠ scripting

And now one of the big issue I have with pretty much any editor today (but more with Neovim as I am actively
contributing to it): if you want to configure your editor, you have to code your configuration **and I highly dislike
that.** It is possible, indeed, to reduce the quantity of code to write, but you will eventually have to get your hands
dirty and write some Lua ([eeew]). And I have a big issue with this.

See, configuration should be stupid and limiting. It should be `key = value`, period. If you need a more structured way
to configure things, just add sections / objects like what you can do with TOML or JSON, but that is all. Remember that
a configuration file might be read by a human, and that you **must** do everything possible to make it super clear what
the configuration state is. Configuring should be:

- Not Turing-complete. You should not have access to logical constructs, such as conditional statements / jump
  statements or anything that would turn your configuration language Turing-complete.
- Imports / includes should not be possible. Having to import things from a configuration file makes it harder for the
  application to read from it and it makes it much harder for people to know what values are currently set.
- If you really want to have “scripting” over your configuration files, use some proper languages made exactly for this
  purpose, such as [Dhall], a template engine or something similar. Leave the choice to the user to decide whether they
  want to configure, or code.

There is a fundamental difference between **having the power to do something and explicitly refusing that power** and
**lacking that power.** I think that configuration _can_ be done via code, but it does not mean it should. The reason
for that is mainly for cognitive complexity reasons. If you are the author of your application, you will very likely
know how to script it. You will know all the quirks and all the little things to know that you will, obviously, have
well documented (right?!). Your users will not have this kind of knowledge. I think it is wrong to use Lua to configure
a software as it will force people to get into the documentation of the Lua API, while all they should have is a list
of configuration options they can set, what setting does what and what are the possible values, **and that is all.**
Providing more power leads to inconsistencies, broken setup and something that is pretty hard to fix without proper
semantic versioning (and an actual package manager enforcing it): a slowly rotting configuration.

Rotting configuration happens when you start piling up lots of “configuration code”, using different APIs, that will
eventually get deprecated or not even used anymore. If you are lucky, the application will just tell you this and that
API calls are deprecated and you should switch to something else. If you are not, you will get some random crashes and
failures in your applications and you will have to debug code. For configuration. And this happens a lot with Neovim.

See, blurring the line between configuration and scripting is easy, especially if you use the same language for both.
Scripting is enhancing the power of an application, such as an editor or a video game, by providing new **behaviors**.
Those behaviors (should them be native to the applications or new ones provided by plugins) are **configured**. If you
start having to script the configuration side, you lose the distinction between both.

At work, we are using k8s a lot. In order to configure our services, we have to deploy configuration massively
everywhere, and we need to generate that configuration. But see, there is a difference between “the configuration” and
“generating the configuration.” The “generate part” is basically a template language. This is code, because it relies on
the datacenter / cluster / environment / feature flags / etc. we want to deploy in / with. However, we do not configure
the software in the template code. We pass fragments of configuration to the template engine, which generates the final
configuration. And those fragments are basically exactly what I described above: key-value objects, sometimes nested to
implement more complex configuration setup, but nothing else. It is basically YAML. If you do not need the complexity of
the datacenter, cluster or environment, you can just run your application with a configuration file that has no code in
it, just a bunch of key-value pairs.

The way Neovim treats configuration right now is via a convention, well-accepted among the plugin developers (and
since I am now officially a Neovim plugin developer with [hop.nvim], I obviously follow it as well). That convention is
the `setup` function plugins expose to pass configuration (via an object often called `opts` for _options_). So people
have to know whether that convention is used, they have to call that function (i.e. it is a regular Lua function call)
and they have to dig in the Lua API. What I think I want to try (I think I am going to work on a plugin for that) is a
way to have a central “configuration store” so that users do not have to go through this `setup` thing anymore — which,
for the record, requires you to do something like `require'the-plugin'.setup { … }`, for every plugin you want to use.
The way I see things would be more like this:

```lua
-- In your init.lua, for instance

require'config-store'.setup {
  telescope = {
    -- options used by telescope
  },
  hop = {
    -- options used by hop
  },
  neogit = {
    -- options used by neogit
  },
  -- etc. etc.
}

require'telescope'
require'hop'
require'neogit'
```

Then, each plugin, in their `init.lua`, could do something like this:

```lua
-- Assuming this is the plugin for hop
local config = require'config-store'.hop or require'hop.config'.defaults
```

This way of doing seems much more natural to me, as it limits the amount of “coding” people have to do to configure
their packages, and it is not really different for package maintainers. Instead of exposing a `setup` function, they can
just get the configuration via this `config-store` thing. I think I will try to experiment with this idea. Obviously, it
requires plugins to adopt this `config-store` thing, and I do think that if people think it is an interesting idea, this
should be a vanilla Lua API and not a random plugin, so that we know this `config-store` object is there, whatever
plugins the user have installed. As discussed above with how we do that at work, I do not want to prevent people from
coding / scripting their editors. But I think a clear distinction between scripting and configuring is important.

# In the end

So those were my (lost) thoughts about the current situation. I might do a follow-up later in the year to see how things
have evolved to me. Today, I stick around Neovim in tmux, using various scripts around fzf to make my shell a bit more
interesting to use. I have to admit that I often open emacs to do three / four commits. The experience is just
consistent. Navigating in projectile, Ivy and magit is unbeaten to me. The simple interactions with dired (to navigate
directories a bit like with a file browser) and edit their contents is something I truly seek everywhere else, even in
my shell. I recently did a quick benchmarked of [Avy](https://github.com/abo-abo/avy), the emacs implementation of
EasyMotion, versus [hop.nvim], and Hop is _really much faster_, so I do not really know what to think (I guess I should
be proud of what I made 🙂). I think that Doom currently has this level of maturity that Neovim will take years to
achieve, not because of talent, but because of the fact Neovim is an editor and has not been designed as being a
productivity platform, while emacs was.

Keep the vibes!

[Neovim]: https://neovim.io
[Doom Emacs]: https://github.com/hlissner/doom-emacs
[Emacs]: https://www.gnu.org/software/emacs
[IntelliJ]: https://www.jetbrains.com/idea
[VS Code]: https://code.visualstudio.com
[Atom]: https://atom.io
[context.vim]: https://github.com/wellle/context.vim
[tmux]: https://github.com/tmux/tmux
[Code::Blocks]: https://www.codeblocks.org
[LSP]: https://microsoft.github.io/language-server-protocol
[hop.nvim]: https://github.com/phaazon/hop.nvim
[EasyMotion]: https://github.com/easymotion/vim-easymotion
[\@gagbo]: https://github.com/gagbo
[\@hlissner]: https://github.com/hlissner
[Telescope]: https://github.com/nvim-telescope/telescope.nvim
[Neogit]: https://github.com/TimUntersberger/neogit
[Ivy]: https://github.com/abo-abo/swiper#ivy
[Helm]: https://github.com/emacs-helm/helm#emacs-helm
[fzf-vim]: https://github.com/junegunn/fzf.vim
[magit]: https://magit.vc
[Octo]: https://github.com/pwntester/octo.nvim
[Forge]: https://magit.vc/manual/forge.html
[Nvim Tree]: https://github.com/kyazdani42/nvim-tree.lua
[Oh My Zsh]: https://ohmyz.sh
[starship.rs]: https://starship.rs/
[eshell]: https://www.gnu.org/software/emacs/manual/html_mono/eshell.html
[projectile]: https://github.com/bbatsov/projectile
[tmux-resurrect]: https://github.com/tmux-plugins/tmux-resurrect
[firefox]: https://www.mozilla.org
[blender]: https://www.blender.org
[gccemacs]: https://www.emacswiki.org/emacs/GccEmacs
[fzf]: https://github.com/junegunn/fzf
[eeew]: https://phaazon.net/media/uploads/eeeeeeeeeeeeeeeeeeeeeeeew.jpg
[Dhall]: https://dhall-lang.org
[toodoux]: https://github.com/phaazon/toodoux
[task-warrior]: https://taskwarrior.org/
