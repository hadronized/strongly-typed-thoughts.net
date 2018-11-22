> This article serves as an announcement for [glsl-0.13].

I’ve been working on [glsl-0.13] for a few days now since my [previous blog entry] about adding
[pest] to [glsl] – and no, the current code is not pest-based, it’s still good old [nom]. Lately,
I’ve been wanting to enhance my [glsl-quasiquote] crate to add [variable interpolation]. I think I
will dedicate a whole article to *variable interpolation* in GLSL because it’s actually tricky to
get done right – without duplicating a whole parser or introducing problematic code with pest (see
my last article).

Since I’m not a programmer who solves problems that don’t exist, I have problems to get resolved in
other crates and binary projects of mine (mostly demoscene and demoscene tooling). However, I’ve
been spending days solving those problems because they’re not related to demoscene only: pretty much
anyone who would like to cope with shaders might be interested.

# The [glsl] crate prior to 0.13

The [glsl] crate provides you with:

  - A GLSL450 parser. I wrote the parser with [nom] back then by implementing the strict OpenGL
    Shading Language Spec ([here](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=2ahUKEwj3uZTS-ebeAhUuyoUKHcW1D04QFjAAegQIAhAC&url=https%3A%2F%2Fwww.khronos.org%2Fregistry%2FOpenGL%2Fspecs%2Fgl%2FGLSLangSpec.4.50.pdf&usg=AOvVaw01MeCZu7SMf3NwCmD83SAr)).
    That parser works mostly on strings and byte slices (I really doubt people use the byte slice
    interface and I might deprecate it).
  - The parser parses into an [Abstract Syntax Tree] – AST for short. That AST is, as the name
    implies, a tree that contains typed *nodes*. It’s a bit more complicated than having a
    `BTreeMap` for instance, because each nodes and levels are completely typed. You can picture the
    AST as both a *type tree* and *value tree* instead of just a *value tree*.
  - Before 0.13, transforming the AST was tedious, because you had to pattern match on the whole
    structure of the AST, and since it’s a *type tree*, it’s really, really boring to do so.
  - One typical transformation that I always need in several crates of mine is *construction*.
    Building ASTs was overwhelming and complicated because you had to create the tree by hand. To
    solve that issue, I introduced the [glsl-quasiquote] crate, that enables you to create the AST
    out of a regular GLSL string. I hit some issues with that, like the impossibility to have typing
    information exchanged between two compiler phases (the procedural macros are all evaluated
    before the rest, making it impossible to have type inference like in `let x: Expr = glsl!{…};`.
    I will try to fix that later.
  - The quasiquote crate works great for static ASTs but as soon as you want to build dynamic ones (
    i.e. that depend on variables for instance), since [glsl-quasiquote] doesn’t have variable
    interpolation (yet!), you’re back to the boring by-hand construction.

[glsl-0.13] comes with several new features and enhancements to partially fix those points that I
will explain in this blog entry.

# The new features!

First, let’s talk about contribution. I received a contribution (both an issue *and* a PR, how
amazing!) from [\@JDemler] about the `#define` preprocessor pragma. Those were not taken into
account so far and he provided the fix to add them! As for all other pragmas, those have to lay in
the global scope – they’re not expected to be found in function bodies, for instance.

Thanks for your contribution! :)

Then, a lot of work has been done around the `glsl::syntax` module. This module contains all the AST
types and some methods were added to, for instance, create *functions*, *variable declarations*,
*if-else* blocks, etc. Those functions were made the most general as possible and heavily depend on
`From` / `Into`, allowing for very short oneliners to create complex AST nodes. The best example
for this is the [`Statement::declare_var`] that declares a new variable as a `Statement`. Most of
the inner AST nodes won’t ever be needed in the public interface, so those functions hide them from
you and give you the easy way to build bigger nodes.

> There is an example below that uses [`Statement::declare_var`]. Keep on reading! :)

Not all AST nodes have helper methods for construction. The good thing is that adding a new function
*shouldn’t* be a breaking change, so I’ll keep adding them as needed – if you have a specific need
for one, feel free to open an issue, I will make the change. That might be surprising but since I
haven’t witnessed lots of people using the crate yet, I don’t implement what I don’t need yet – but
as soon as someone tells me they need something, either I immediately review the PR, or plan some
time to make the patch myself.

Finally, the biggest feature which will be explained in further details in this blog post: **AST
visitors**. AST visitors are the way I imagined would be the best to traverse an AST in order to
mutate some nodes, all the nodes, filter, query information, etc. It’s like *lenses* for [glsl]! –
but trust me: it’s way lighter! :D

## Visiting ASTs

AST visitors solve a problem I had when implementing a specific feature in [spectra]. I needed to be
able to change **all** the references to a given identifier in all the AST at once. Either the
identifier is used in an expression assigned to a new variable, returned from a function or passed
as argument to a function, I needed to catch that identifier… and change its name.

That was quite of a desperate challenge without a proper solution. Imagine: I would have to write
the code to change the identifier myself and… find ALL the places where identifiers *might* appear
in *any* AST. This is possible, but that would drive many developers mad – especially whevener the
[glsl] crate changes.

So I came up with a better solution. I will not drop you a technical term and have you read
Wikipedia, I would dislike that. I will just explain from bottom up why it’s designed this way and
why I think it’s the best solution.

## Visiting summarized

The idea is that pattern-matching on an identifier might appear in several places in an AST. So
whatever solution we choose, we will have to find all those spots and call a function that states:

> *Hey there. Here is an `Identifier`. I give you a `&mut Identifier` so that you can change it.*

An `Identifier` is an AST (a really simple one, but still is). So you might want to implement that
function on `Identifier`… but what happens when your AST is an `Expr`, and that expr is a variable –
that is, `Expr::Var(identifier_here)`? You will want to implement your function on `Expr` *also*,
then. And here you see that you need a trait, because, as said earlier, the AST is a *type tree*.

However, what trait? We could imagine implementing that trait only on AST types that have an
`Identifier` as field or variant. But the most general AST node, `TranslationUnit`, is a non-empty
list of `ExternalDeclaration`s. If we want to visit that, we won’t be able to type match and pass
down our function transforming identifiers.

We see that we will need to implement that trait for all AST types so that they can pass the
function down the AST to a node that actually has an `Identifier`.

And since we’re doing it with `Identifier`, we might want to do it with *any* AST node. But if we
do that, we cannot pass one single function anymore…

## The Visitor

So you need an object that will be able to treat an `Identifier`… or a `TranslationUnit`, or
anything AST. This is a bit boring to implement, but we need a trait that enables a type to visit
any AST node:

```
trait Visitor {
  fn visit_identifier(&mut self, _: &mut Identifier);
  fn visit_translation_unit(&mut self, _: &mut TranslationUnit);
  // …
}
```

This is great, because if a type implements `Visitor`, it means that we can call `visit_identifier`
on it if we have an `Identifier`! When we will be implementing our traversing function, we will just
have to carry around a mutable reference to an object implementing `Visitor`, and call the right
function depending on the AST node / type we are at!

We also use mutable references here so that we can also mutate information as we sink into the AST.
This might be very useful to know at which block depth we are at, or if we’re in a function’s body,
etc.

Something important with that trait though: the current implementation (from this article) would be
very boring to implement, because it has a lot of `visit_*` methods. What if we’re only interested
in `Identifier`? We don’t want to have to implement `visit_translation_unit` because we don’t care.

A simple fix to that is to give all those `visit_*` methods a default implementation… that does
nothing.

```
trait Visitor {
  fn visit_identifier(&mut self, _: &mut Identifier) {}
  fn visit_translation_unit(&mut self, _: &mut TranslationUnit) {}
  // …
}
```

Let’s try it!

```
struct ReplaceIdentifier<'a> {
  replacement: &'a str
}

impl<'a> Visitor for ReplaceIdentifier<'a> {
  fn visit_identifier(&mut self, ident: &mut Identifier) {
    *ident = self.replacement.clone().into()
  }
}
```

And that’s all! We now have a visitor that can be used to traverse any AST and change any
`Identifier` to what we have set. For instance:

```
let mut ast = …;
let mut visitor = ReplaceIdentifier { replacement: "foobar" }

// wait, how do we pass the visitor to the AST here?
```

Argh, we’re still missing something!

## Hosting visitors

We need a `visit` function, like:

```
fn visit<V>(ast: &mut TranslationUnit, visitor: &mut V) where V: Visitor
```

So let’s write it!

```
fn visit<V>(ast: &mut TranslationUnit, visitor: &mut V) where V: Visitor {
  for external_decl in ast {
    // ah.
  }
}
```

We cannot call `visit` again on `ExternalDeclaration`. Seems like we need another trait! :D

```
trait Host {
  fn visit<V>(&mut Self, visitor: &mut V) where V: Visitor;
}
```

Here, a type that implements `Host` means that it can call a `Visitor` on itself, and might pass it
down to children AST if any. Since we’re going to implement `Host` for all our AST types, we will be
able to do something like this:

```
impl Host for TranslationUnit {
  fn visit<V>(&mut Self, visitor: &mut V) where V: Visitor {
    visitor.visit_translation_unit(self); // first, we have the visitor visit the AST node

    // then, for all children, we pass down the visitor!
    for external_decl in self {
      external_decl.visit(visitor);
    }
  }
}
```

And here we go. We are able to pass down the visitor, that will be called for each node. If you have
provided an implementation for a given `visit_*`, it will get invoked, otherwise, the default
implementation will fire – and it does nothing.

A simple optimization can be done, here. Since you might *know* that you’re done at a given level
regarding your visitor, we could add a way to make a `Host` stop visiting and go any deeper. For
this, we introduce a simple enum:

```
enum Visit {
  Children, // keep visiting children
  Parent // stop visiting, go back to parent
}
```

And we change all the `Visitor` methods to return a `Visit`. That will give us the information we
need when implementing `Host::visit` now:

```
impl Host for TranslationUnit {
  fn visit<V>(&mut Self, visitor: &mut V) where V: Visitor {
    let visit = visitor.visit_translation_unit(self); // first, we have the visitor visit the AST node

    if visit == Visit::Children {
      // then, for all children, we pass down the visitor!
      for external_decl in self {
        external_decl.visit(visitor);
      }
    }
  }
}
```

We also need to change the default implementation of the `visit_*` methods. My choice was to have
them return `Visit::Children` by default, because I think it’s a saner default – if people don’t
want to go any deeper in the AST, they will just have to dummy-implement the right method.

And we are done! The actual implementation is a bit more complex than that but is really really
close to what is described in this article. I’ll give you the example from the official
documentation so that can you can see how it’s *really* used:

```
use glsl::syntax::{CompoundStatement, Expr, SingleDeclaration, Statement, TypeSpecifierNonArray};
use glsl::visitor::{Host, Visit, Visitor};
use std::iter::FromIterator;

let decl0 = Statement::declare_var(
  TypeSpecifierNonArray::Float,
  "x",
  None,
  Some(Expr::from(3.14).into())
);

let decl1 = Statement::declare_var(
  TypeSpecifierNonArray::Int,
  "y",
  None,
  None
);

let decl2 = Statement::declare_var(
  TypeSpecifierNonArray::Vec4,
  "z",
  None,
  None
);

let mut compound = CompoundStatement::from_iter(vec![decl0, decl1, decl2]);

// our visitor that will count the number of variables it saw
struct Counter {
  var_nb: usize
}

impl Visitor for Counter {
  // we are only interested in single declaration with a name
  fn visit_single_declaration(&mut self, declaration: &mut SingleDeclaration) -> Visit {
    if declaration.name.is_some() {
      self.var_nb += 1;
    }

    // do not go deeper
    Visit::Parent
  }
}

let mut counter = Counter { var_nb: 0 };
compound.visit(&mut counter);
assert_eq!(counter.var_nb, 3);
```

# Future work

The current state of `Visitor` is great but there is a drawback: your AST has to be mutable. You
might want to only traverse it read-only but have your visitor mutated. I might then modify slightly
the `Visitor` trait and `Host` one with `Host::visit_mut` if I think it’s needed.


Feel free to experiment around. Next work planned for [glsl] – besides contributions – quasiquoting
variable interpolation.

Keep the vibes!

[glsl]: https://crates.io/crates/glsl
[glsl-0.13]: https://crates.io/crates/glsl/0.13.2
[glsl-quasiquote]: https://crates.io/crates/glsl-quasiquote
[previous blog entry]: https://phaazon.net/blog/glsl-pest-part-2
[pest]: https://crates.io/crates/pest
[nom]: https://crates.io/crates/nom
[variable interpolation]: https://en.wikipedia.org/wiki/String_interpolation
[Abstract Syntax Tree]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
[\@JDemler]: https://github.com/JDemler
[`Statement::declare_var`]: https://docs.rs/glsl/0.13.2/glsl/syntax/enum.Statement.html#method.declare_var
[spectra]: https://crates.io/crates/spectra
