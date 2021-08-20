# phaazon.net front-end

This directory holds the front-end of phaazon.net. This is a [Halogen]-based web application, written in [PureScript]
and targetting the phaazon.net’s backend, written in [Haskell].

## Architecture

The code is split into several modules, which implement one different _component_, in order to form a tree which itself
represents the application:

- `SPA.purs`: the top-level component of the application, responsible in switching the active current component.
  - `AboutMe.purs`: a small introduction to who I am.
  - `Blog.purs`: blog component, responsible in listing all the articles, searching through them, displaying them.
  - `Browse.purs`: (public) file browser, allowing to browse various uploaded files.

[Halogen]: https://github.com/purescript-halogen/purescript-halogen
[PureScript]: https://www.purescript.org
[Haskell]: https://www.haskell.org
