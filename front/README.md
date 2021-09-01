# phaazon.net front-end

This directory holds the front-end of phaazon.net. This is a [Halogen]-based web application, written in [PureScript]
and targetting the phaazon.net’s backend, written in [Haskell].

## Architecture

The code is split into several modules, which implement one different _component_, in order to form a tree which itself
represents the application:

- `API.purs`: API utility functions to build endpoints, call APIs, etc.
- `HTMLHelper.purs`: some functions used when building HTML trees.
- `Main.purs`: entrypoint of the application.
- `Router.purs`: front-end routing features used to manipulate (get and set) URL’s paths in the browser.
- `component/`: list all Halogen components used by the web application.
  - `AboutMe.purs`: a small introduction to who I am.
  - `Blog.purs`: blog component, responsible in listing all the articles, searching through them, displaying them.
  - `Browse.purs`: (public) file browser, allowing to browse various uploaded files.
  - `Child.purs`: bunch of useful types and functions components can use to communicate with child components.
  - `SPA.purs`: the top-level component of the application, responsible in switching the active current component.

[Halogen]: https://github.com/purescript-halogen/purescript-halogen
[PureScript]: https://www.purescript.org
[Haskell]: https://www.haskell.org
