For the last month, I had technical talks with plenty of programmers from all around the globe – thank
you IRC! Something interesting that often showed up in the discussion was the actual *workflow* we have
when writing code. Some people are use to [IDE] and won’t change their tools for anything else. Some
others use very basic text editors (I even know someone using [BBEdit] for his job work! crazy,
isn’t it?). I think it’s always a good thing to discuss that kind of topic, because it might give
you more hindsight on your own workflow, improve it, share it or just show your nice color scheme.

I’ll be talking about:

# My editor

I’ve tried a lot of editors in the last ten years. I spent a year using emacs but eventually
discovered – erm, learned! – vim and completely fell in love with the *modal* editor. It was
something like a bit less than ten years ago. I then tried a lot of other editors (because of
curiosity) but failed to find something that would help be better than vim to write code. I don’t
want to start an editor war; here’s just my very personal point of view on editing. The concept of
modes in vim enables me to use a very few keystrokes to perform what I want to do (moving, commands,
etc.) and I feel very productive that way.

> A year ago, a friend advised me to switch to [neovim], which I have. My editor of the time is then
> neovim, but it’s so close to vim that I tend to use (neo)vim. :)

I don’t use any other editing tool. I even use neovim for taking notes while in a meeting or when I
need to format something in Markdown. I just use it everywhere.

# My neovim setup

I use several plugins:

```
Plugin 'VundleVim/Vundle.vim'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'gruvbox'
Plugin 'neovimhaskell/haskell-vim'
Plugin 'itchyny/lightline.vim'
Plugin 'rust-lang/rust.vim'
Plugin 'jacoborus/tender.vim'
Plugin 'airblade/vim-gitgutter'
Plugin 'tikhomirov/vim-glsl'
Plugin 'plasticboy/vim-markdown'
Plugin 'cespare/vim-toml'
Plugin 'mzlogin/vim-markdown-toc'
Plugin 'ElmCast/elm-vim'
Plugin 'raichoo/purescript-vim'
Plugin 'easymotion'
Plugin 'scrooloose/nerdtree'
Plugin 'ryanoasis/vim-devicons'
Plugin 'tiagofumo/vim-nerdtree-syntax-highlight'
Plugin 'mhinz/vim-startify'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'tpope/vim-fugitive'
Plugin 'MattesGroeger/vim-bookmarks'
Plugin 'luochen1990/rainbow'
```

## Vundle

A package manager. It just takes the list you read above and clones / keeps updated the git
repositories of the given vim packages. It’s a must have. There’re alternatives like Pathogen but
Vundle is very simple to setup and you don’t have to care about the file system: it cares for you.

## ctrlp

This is one is a must-have. It gives you a fuzzy search buffer that traverse files, MRU, tags,
bookmarks, etc.

![](http://phaazon.net/pub/ctrlp.png)

I mapped the file search to the `, f` keys combination and the tag fuzzy search to `, t`.

## gruvbox

The colorscheme I use. I don’t put an image here since you can find several examples online.

> This colorscheme also exists for a lot of other applications, among terminals and window managers.

## haskell-vim

Because I write a lot of Haskell, I need that plugin for language hilighting and linters… mostly.

## lightline

The [Lightline] vim statusline. A popular alternative is [Powerline] for instance. I like Lightline
because it’s lightweight and has everything I want.

## rust.vim

Same as for Haskell: I wrote a lot of Rust code so I need the language support in vim.

## tender

A colorscheme for statusline.

## vim-gitgutter

I highly recommend this one. It gives you diff as icons in the symbols list (behind the line numbers).

![](http://phaazon.net/pub/vim-gitgutter.png)

## vim-glsl

GLSL support in vim.

## vim-markdown

Markdown support in vim.

## vim-toml

TOML support in vim.

> TOML is used in Cargo.toml, the configuration file for Rust projects.

## vim-markdown-toc

You’re gonna love this one. It enables you to insert a table of contents wherever you want in a
Markdown document. As soon as you save the buffer, the plugin will automatically refresh the TOC for
you, keeping it up-to-date. A must have if you want table of contents in your specifications or RFC
documents.

## elm-vim

Elm support in vim.

## purescript-vim

Purescript support in vim.

## easymotion

A must have. As soon as you hit the corresponding keys, it will replace all words in your visible
buffer by a set of letters (typically one or two), enabling you to just press the associated
characters to jump to that word. This is the *vim motion* on steroids.

## nerdtree

A file browser that appears at the left part of the current buffer you’re in. I use it to discover
files in a file tree I don’t know – I use it very often at work when I work on a legacy project, for
instance.

## vim-devicons

A neat package that adds icons to vim and several plugins (nerdtree, ctrlp, etc.). It’s not a must-have
but I find it gorgeous so… just install it! :)

## vim-nerdtree-syntax-highlight

Add more formats and files support to nerdtree.

## vim-startify

A *cute* plugin that will turn the start page of vim into a MRU page, enabling you to jump to the
given file by just pressing its associated number.

> It also features a cow that gives you fortune catchphrases. Me gusta.

## nerdtree-git-plugin

Git support in nerdtree, it adds markers in front of file that have changes, diff, that were added,
etc.

## vim-fugitive

A good Git integration package to vim. I use it mostly for its `Gdiff` diff tooling directly in vim,
even though I like using the command line directly for that. The best feature of that plugin is
the integrated blaming function, giving you the author of each line directly in a read-only vim
buffer.

## vim-bookmarks

Marks on steroids.

## rainbow

This little plugins is very neats as it allows me to add colors to matching symbols so that I can
easily see where I am.

![](http://phaazon.net/pub/rainbow_vim.png)

# Workflow in Rust

I’m a rustacean. I do a lot of Rust on my spare time. My typical workflow is the following:

1. I edit code in neovim
2. Depending on the project (whether I have a robust unit tests base or not or whether I’m writing a
   library or an app), I use several `cargo` commands:
   - for a library project, I split my screen in two and have a `cargo watch -x test` running; this
     command is a file watcher that will run all the tests suites in the project as soon as a file
     changes;
   - for an app project, I split my screen in two and have a `cargo watch` – similar to `cargo watch -x check`
     running; this command is a file watcher that will proceed through the compilation of the binary
     but it will not compile it down to an actual binary; it’s just a check, it doesn’t produce
     anything you can run. You can manually run that command with `cargo check`. See more
     [here](https://github.com/rust-lang/cargo/pull/3296#event-893283611);
   - for other use cases, I tend to run `cargo check` by hand and run `cargo build` to test the
     actualy binary
3. Because I’m a unix lover, I couldn’t work correctly without a terminal, so I use neovim in a
   terminal and switch from the console to the editor with the keybinding `C-z` and the `jobs` and
   `fg` commands. I do that especially to create directories, issue git commands, deploy things,
   etc. It’s especially ultra cool to run a `rg` search or even a `find` / `fd`. I sometimes do that
   directly in a neovim buffer – with the `:r!` command – when I know I need to edit things from the
   results. Bonus: refactoring with `tr`, `sed` and `find -exec` is **ultra** neat.

# Workflow in Haskell

The workflow is almost exactly the same besides the fact I use the `stack build --fast --file-watch`
command to have a file watcher.

> Haskell’s stack doesn’t currently the awesome `check` command Rust’s cargo has. Duh :(

I also have a similar workflow for other languages I work in, like Elm, even though I use standard
unix tools for the file watching process.

# Git workflow

Aaaah… `git`. What could I do without it? Pretty much nothing. `git` is such an important piece of
software and brings such an important project philosophy and behaviors to adopt.

What I find very cool in git – besides the tool itself – is everything around it. For instance, on
GitHub, you have the concept of Pull Request (PR) – Merge Request in GitLab (MR). Associated with a
good set of options, like disallowing people to push on the `master` branch, hooks, forcing people
to address any comments on their MR, this allows for better code reviews and overall a way better
quality assurance of the produced code than you could have without using all this infrastructure.
Add a good set of DevOps for deployment and production relatide issues and you have a working team
that has no excuse to produce bad code!

## Some git candies I love working with

My neovim fugitive plugin allows me to open a special buffer with the `:Gblame` command that gives
me a `git blame` annotation of the file. This might be trivial but it’s very useful, especially at
work when I have my colleagues next me – it’s always better to directly ask them about something
than guessing.

![](http://phaazon.net/pub/vim_fugitive_blame.png)

Another one that I love is `:Gdiff`, that gives you a `git diff` of the modification you’re about to
stage. I often directly to a `git diff` in my terminal, but I also like how this plugin nails it as
well. Very pratictal!

![](http://phaazon.net/pub/vim_fugitive_diff.png)

# General note on workflow

It’s always funny to actually witness difference in workflows, especially at work. People who use
mostly IDEs are completely overwhelmed by my workflow. I was completely astonished at work that some
people hadn’t even heard of `sed` before – they even made a Google search! I’m a supporter of the
philosophy that one should use the tool they feel comfortable with and that there’s no “ultimate”
tool for everyone. However, for my very own person, I really can’t stand IDEs, with all the buttons
and required clicks you have to perform all over the screen. I really think it’s a waste of time,
while using a modal editor like neovim with a bépo keyboard layout (French dvorak) and going back
and forth to the terminal is just incredibly simple, yet powerful.

I had a pretty good experience with [Atom], a modern editor. But when I’ve been told it’s written
with web technologies, the fact it’s slow as f*ck as soon as you start having your own tooling
(extensions), its pretty bad behavior and incomprensible “Hey, I do whatever the f*ck I want and
I’ll just reset your precious keybindings!” or all the weird bugs – some of my extensions won’t just
work if I have an empty pane open, wtf?!… well, I was completely bolstered that GUI interfaces, at
least for coding and being productive, are cleary not for me. With my current setup, my hands
*never* move from the keyboard – my thrists are completely static. With all the candies like
easymotion, ctrlp, etc. etc. I just can’t find any other setups faster and comfier than this one.

There’s even an extra bonus to my setup: because I use mostly unix tools and neovim, it’s pretty
straigth-forward to remote-edit something via ssh, because everything happens in a terminal. That’s
not something you can do easily with Atom, Sublime Text or any other editors / IDEs – and you even
pay for that shit! No offence!

However, there’s a caveat: because pretty much everything I do is linked to my terminal, the user
experience mostly relies on the performance of the terminal. Using a bad terminal will result in an
overall pretty bad experience, should it be editing, compiling, git or ssh. That’s why I keep
lurking at new terminal emulaters – [alacritty] seems very promising, yet it’s still too buggy and
lacks too many features to be production-ready to me – but it’s written in Rust and is
GPU-accelerated, hells yeah!

# Conclusion

Whoever you are, whatever you do, whomever you work with, I think the most important thing about
workflow is to find the one that fits your needs the most. I have a profound, deep digust for
proprietary and closed software like Sublime Text and IDEs that use GUIs while keyboard shortcut are
just better. To me, the problem is about the learning curve and actually wanting to pass it –
because yes, learning (neo)vim in details and mastering all its nits is not something you’ll do in
two weeks; it might take months or years, but it’s worth it. However, as I said, if you just feel
good with your IDE, I will not try to convert you to a modal editor or a unix-based workflow…
because you wouldn’t be as productive as you already are.

Keep the vibe!

[IDE]: https://en.wikipedia.org/wiki/Integrated_development_environment
[BBEdit]: https://www.barebones.com/products/bbedit
[neovim]: https://neovim.io
[Lightline]: https://github.com/itchyny/lightline.vim
[Powerline]: https://github.com/powerline/powerline
[Atom]: https://atom.io
[alacritty]: https://github.com/jwilm/alacritty
