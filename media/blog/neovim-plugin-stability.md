Have you ever installed a Neovim plugin and realized a few days / weeks later, after updating the
plugin, that something broke? That you now have have annoying error messages at startup? If so, then
this blog article should ring a bell. If not, it will probably open your eyes on a problem that can
bite you at any moment.

# Neovim and plugins

I have been in that situation where I struggle with plugins breaking every time I update, and not
only in Neovim. The problem can occur pretty much anywhere. The main reason to this is simple:
_breaking changes_ happen, and must happen. Plugins maintainers might decide at some point that
something must be removed, or changed. A configuration option you were using disappears or its
name changes. That happens and it’s part of the how software works.

However, there is a difference between breaking changes in plugins and breaking changes in i.e.
libraries / binaries we write in languages supporting package managers, such as Rust and `cargo`,
Haskell and `cabal`, Python and `poetry`, etc. etc. Actually, there are two main differences,
depending on the perspective you take to look at the problem:

- From a user perspective, software is not versioned and « they want the last updated thing. »
  Obviously, if you are a programmer, you know that it’s likely something is versioned, but most
  of the time, when you install something like a video game you like, an editor, a plugin or
  anything, do you really ask for a specific version? There are obviously situations where you
  need to ensure the versions that you use, but most of the time, _people do not care_.
- From a plugin author perspective, there is no tooling to ensure that you can ship a feature
  without breaking your users.

About the last point, when I work on a given software project, I specify the dependencies of my
project, and I specify the versions of those dependencies. Using [SemVer], I’m sure that if the
those dependencies are correctly written, I should be able to release my software (lib or bin)
without any issues on my users. There are exceptions, but those are rare and most of the time
related to human errors when implementing [SemVer] incorrectly.

When you write a plugin, you will most likely write it in a language and ecosystem that doesn’t even
support versioning. Which brings me to the main point of this article: how do you prevent breaking
your users?

# The git channel and the « latest » problem

Neovim plugins are often implemented in package managers like
[packer] via Git URLs. Most of the time, `foo/bar` will be installed as the `bar` plugin, part of
the org / user `foo` on GitHub. You can also install local plugins but that’s off topic here. When
you provide the description of the plugin to install, you will most of the time provide nothing more.
This will default to install and synchronize the plugin using the `master` branch (or whatever your
local `git` says, like `main`). As a Software Engineer, to me, this is a really bad habit we all
have, and I’ll explain why.

When you depend on the _latest_ version of something, you are basically signing a contract between
your local system and the remote system to keep your local version in sync with remote. If the
remote gets updated, you get the changes the next time you update. The problem with this is that
`master` contains zero information about whether an update is a breaking change or not. It’s the
same reason why defaulting API calls to your API service to the latest version of the API is _wrong_
and that you should default them to `v1`: because it will never change, so people have to _opt-in_
for breaking changes instead, which is a much saner way of dealing with breaking changes.

So I’ve been thinking about this problem for quite a while now. In ecosystem like Rust, crates have
to implement [SemVer], so that `cargo`, the package manager and build system, knows how to resolve
the dependency graph given your version requirements. But we don’t have that with Neovim plugins… or
do we?

# The solution is (partially) already there

In [packer] (and probably others), we can provide additional information to download (and update)
plugins. For instance, [packer] supports the `branch = …`, `tag = …` and `commit = …` configuration
options. So this gave me an idea. And this idea is something I would like to turn into a proposal
for the Neovim community. The idea is to use both branches and tags to encode [SemVer] information
and allow users to specify plugins a bit like you would specify dependencies in tools like `cargo`.
In order to do that, plugin authors must accept and implement [SemVer]. The current way I see things
is actually pretty simple:

- `master`, `main`, etc. can be used as a _nightly_ channel. Users who don’t want to deal with this
  [SemVer] idea can just ignore the whole thing and still use their plugins the way they’ve always
  had.
- A triplet git tag, `vM.m.p`, represents a full, immutable version that cannot be changed. This
  would allow a user to depend on `vM.m.p` and be sure that the plugin would be pinned to that
  version. Here, `M` is the major version, `m` is the minor version and `p` is the patch version.
  Every time the plugin is modified in a way that doesn’t show on the interface (no breaking change
  and no non-breaking change, only internal patches), the `p` version is incremented.
- A pair git branch `vM.m`, with `M` the major and `m` the minor version. The idea is that every
  time a plugin gets a new non-breaking change, the `m` version is incremented. That would allow
  users to depend on `vM.m` to be sticky to that minor version, but still receive patches updates.
  More on that below.
- A single version git branch, `vM`, with `M` the major version. Every time the plugin gets a new
  breaking change, `M` is incremented. This would probably be the most interesting options for
  users, as it would allow them to benefit from new features without having their plugin break on an
  update.

In order to implement this scheme using git branches and git tags, there is one trick to implement.
One a plugin author decide to make a new release, they have to do several things:

- Create a git tag. If the previous version was `v1.2.3`, they will either create `v1.2.4`, `v1.3.0`
  or `v2.0.0`, depending on the kind of change.
- If they have created a new patch version:
  - They need to create the tag `v1.2.4`.
  - They need to update the branch `v1.2` so that it now points to the same commit as `v1.2.4`.
  - They need to update the branch `v1` so that it now points to the same commit as `v1.2.4`.
- If they have created a new minor version:
  - They need to create the tag `v1.3.0`.
  - They need to create the branch `v1.3` and make it point to the same commit as `v1.3.0`.
  - They need to update the branch `v1` so that it now points to the same commit as `v1.3.0`.
- IF they have created a new major version:
  - They need to create the tag `v2.0.0`.
  - They need to create the branch `v2.0` and make it point to the same commit as `v2.0.0`.
  - They need to create the branch `v2` and make it point to the same commit as `v2.0.0`.

With this approach, a user can now depend on a version and be sure not to get breaking-changes. An
example for my config for [hop.nvim]:

```lua
use {
  'phaazon/hop.nvim',
  branch = "v1",
  config = function()
    require'hop'.setup {
      keys = 'etovxqpdygéèfblzhckisuran',
    }
  end
}
```

# Tooling

I’m currently writing a (small) plugin, branching on package managers such as [packer], to be able,
from a user perspective, to get a listing of packages that can be upgraded. For instance, if you
depend on `telescope.nvim-0.4`, if `telescope.nvim-0.4.9` is released, [packer] should pick it up
because you would have `branch = v0.4` in your config (and that branch would be updated to point to
git tag `v0.4.9`). However, if `telescope-nvim-0.5` is released, that tool will provide you with a
hint that a new version is available and that you need to manually select it, because it might break
your configuration.

# Pros. & cons.

The obvious advantage (and incentive) here is that if plugin authors accept to implement something
like this, plugin stability will be an old bitter memory. People who don’t care and want to live on
the bleeding edge can completely ignore this proposal and still use the `master` / `main` branch
(not providing the branch unfortunately often defaults to the `master` branch). The other incentive
here is that plugins (like the one I’m writing for the tooling) can now do more things with git tags
and branches to display more information, like the number of releases, git tag annotations to show
proper release notes inside Neovim, etc.

The drawbacks are not negligible: implementing such a [SemVer] proposal using git tags and branches
is going to require a bit more work, and the tooling is clearly lacking. This is very similar to how
dynamic relocatable objects (`.so`) are handled on most Linux systems: the actual `.so` file is
versioned somewhere on your file system, like `/usr/lib/foo.1.2.3.so`, and applications / other
libraries can depend on major and minor versions by using _symlinks_: `/usr/lib/foo.1.so` points to
`/usr/lib/foo.1.2.so`, which in turns points to `/usr/lib/foo1.2.3.so`. `git` doesn’t support sticky
branches (i.e. you can’t ask to make a branch reference another one, it has to reference a commit,
which is always immutable), so it means that updating a version requires you to update the whole
hierarchy of branches, which is a bit annoying. A tool (like a shell tool in the plugin I’m writing)
could probably help with that.

So what do you think? Worth it? I think that in terms of stability, it is a missing piece of pretty
much any editor supporting dynamic plugins. The « ideal » situation would be to support semantic
versioning directly at the package level (which is not [packer], but directly how Neovim represents
package), and the actual encoding of the package versions would probably be hard to implement using
only a centralized system like GitHub as there is no index (besides git tags and branches).

[SemVer]: https://semver.org
[packer]: https://github.com/wbthomason/packer.nvim
[hop.nvim]: https://github.com/phaazon/hop.nvim
