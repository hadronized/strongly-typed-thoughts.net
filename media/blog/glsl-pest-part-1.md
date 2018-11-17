This is the first article out of a (I guess?!) series of upcoming articles about… *parsing*. More
specifically, I’ve been writing the [glsl] crate for a while now and
the current, in-use parser is backed with [nom]. [nom] is a [parser combinator] crate written
originally by [\@geal] and there has been a lot of fuzz around [nom] vs. [pest] lately.

Soooooooooooo. Because [glsl] is written with [nom] in its third iteration and because [nom] is now
at version `4`, I decided it was time to update the parser code of [glsl]. I heard about the
comparison between [pest] and [nom] and decided to write an implementation with [pest].

This is the first article of a series about how writing a [pest] looks like is fun compared to
writing the [nom] parser. I’ll post several articles as I advance and see interesting matter to
discuss and share.

  - Second article [here](https://phaazon.net/blog/glsl-pest-part-2)

# Oh my; I love PEG!

First thing first: [pest] consumes a file at compile-time that contains a [PEG] grammar defining the
format of input you want to parse. [PEG] – which stands for *Parsing Expression Grammar* – is a
formal language that enables you to describe your own language with *rules*. Those rules are written
with some basic blocks that belong to *Language Theory* – if you’ve ever heard of Stephen Kleene and
its famous [Kleene star] for instance, you will feel familiar with PEG.

What I love about PEG is that with a very limited set of constructs, you can describe a lot of
determinist languages. In the case of GLSL450 – which is the language that the [glsl] crate can
parse – it’s a *context-free* and *determinist* language. So the whole language can be defined in
terms of (recursive) PEG rules.

The PEG primitive constructs available in [pest] are:

  - Matching against a lexeme, like `"foo"`: this rule will recognize any string that is `"foo"`.
  - Matching against a range of char, like `'a' .. 'z'`: this will recognize any char in the range,
    like `"a"`, `"d"` or `"z"`.
  - Matching against another rule, simply by using its name.
  - Matching a sequence of rules, like `a ~ b`: this rule will be matched if it can match the `a`
    rule followed by the `b` rule; `"foo" ~ 'a' .. 'z'` will recognize `"foot"`, `"food"`, `"fooz"`
    but also `"foo r"` – this whitespace behavior is explained below – etc.
  - Matching either a rule or another (alternative), like `a | b`: this rule will match either `a`
    if it succeeds or `b` if `a` fails and `b` succeeds or none otherwise. `"foo" | 'a' .. 'z'` will
    be matched with `"foo"` and `"a"` or `"f"`.
  - Optional matching with the `?` suffix operator, like `a?`: this will match `a` if it succeeds or
    will just ignore its failure if it fails, making it optional. For instance, `"hello"? ~ "world"`
    will match both `"hello world"` and `"world"`.
  - Parenthesis can be used to group rules and reason on them as a whole, like `(a ~ b)`.
  - Repetitions:
    - The [Kleene star] (matches zero or many), like `a*`: this rule matches zero or more times the
      `a` rule. For instance, `('0' .. '9')*` will match `"0"` and `"314"` but will also match `""`.
    - The [Kleene plus] (matches one or many), like `a+`: this rule matches one or more times the
      `a` rule. For instance, `('0' .. '9')+` will match `"0"` and `"314"` but will not match `""`
      because it expects at least one digit.
    - The `{min, max}` notation. The rule `a{0,}` is akin to `a*`: it must be matched at least 0
      times and at maximum infinite times. The rule `a{,3}` must be matched between 0 and 3
      times and the rule `a{1, 12}` must be matched between 1 and 12 times.

Additionally, [pest] provides a notation to add *rule modifiers*:

  - A simple rule is written `rule_name { definition }`, like `number = { ASCII_DIGIT+ }`.
  - `~`, `*` and `+` rules get interleaved with two special rules **if defined**:
    - `WHITESPACE`, defining how to eat a single whitespace.
    - `COMMENT`, defining how to eat a comment.
    - Those rules are interleaved as `(WHITESPACE | COMMENT)*` so that `number = { ASCII_DIGIT+ }`
      gets actually rewritten as `number = { (ASCII_DIGIT | (WHITESPACE | COMMENT)*)+ }`.
  - Because of that last rule, sometimes we don’t want that to happen. In the case of the `number`
    rule defined above, both `"324"` and `"3 2 4"` will get matched if we defined `WHITESPACE` to
    eat `" "`. In order to fix this, the `@` modifier can be prepended to the rule definition:
      - `number = @{ ASCII_DIGIT+ }`
      - This rule will not get interleaved at all and is *atomic*.
      - Easy-to-remember: `@` for *@tomic*. :)
  - The `_` modifier can be used to mute a rule. For instance, eating whitespaces will not provide
    you with any useful tokens. That’s why the `WHITESPACE` rule is defined as
    `WHITESPACE = _{ " " }`.
  - There are some other modifiers, like `$` and `!` but we’re not interested in them as they’re
    very specific.

So, basically, you write a (long) list of PEG rules in a file, give that file to Rust via a
proc-macro derive, *et voila*!

```
#[derive(Parser)]
#[grammar = "path/to/grammar.pest"]
struct Parser;
```

It’s that simple.

# Facing issues

PEG is really cool to use… but there are caveats. For instance, PEG / [pest] cannot express
*left-recursive* rules. To understand this, take this rule:

```
foo = { a | b ~ foo* }
```

This rule is correct and will allow you to match against `"a"` or `"b"` or even `"bbbbbbbbb"` and
`"bbbbbbbba"`. However, the following one is left-recursive and won’t be able to be compiled:

```
foo = { a | foo* ~ b }
```


As you can see, the second alternative is left-recursive. Imagine matching against `"a"`: the first
alternative fires, everything is great. Now match against `"b"`. The first alternative fails so
[pest] tries the second one, but in order to take the second one, it needs to try at least once
`foo` (the `foo*`) to see whether it fails or not. If it fails, it will then ignore and try to eat a
`"b"`. If it succeeds, it will try to fire the `foo` rule again. However, when it tries `foo` for
the first time, it will try the first alternative again, `a`, then will fail, then will try `foo*`,
then `a`, then `foo*`, then… You see the pattern.

Left-recursive rules are then forbidden so that we can escape that situation. PEGs need to be
unambiguous and the rules (especially alternatives) are tested **in order**. You will have to unroll
and optimize those rules by hand. In our case, we can rewrite that rule in a PEG-compliant way:

```
foo = { (a | b)+ }
```

An issue I had while writing the PEG of GLSL450 was with the `expression` rule (among a loooot of
others). Khronos’ specifications are written with left-recursive rules pretty much everywhere and
this is the one for expressions:

```
expression:
  assignment_expression
  expression COMMA assignment_expression
```

Read it as *“An expression is either an assignment expression or an expression with a comma followed
by an assignment expression”*. This actually means that the second alternative cannot be constructed
without the first one – because it’s recursive! So you must start with an `assignment_expression`.
Then you can have the second rule applied recursively, like so (parenthesis just for convenience to
show the left-recursion):

```
((((assignment_expression) COMMA assignment_expression) COMMA assignment_expression) COMMA assignment_expression)
```

Etc., etc. If you remove the parenthesis, you get:

```
assignment_expression COMMA assignment_expression COMMA assignment_expression COMMA assignment_expression
```

Which you should now be able to write with a PEG rule easily! Like so:

```
expression = { assignment_expression ~ (COMMA ~ assignment_expression)* }
```

Way better and without left-recursion! :) So I spent pretty much a whole afternoon trying to remove
left-recursions (and some rules were really nasty). I even have one remaining rule I’ll have to cut
like a surgeon later to make it fully available (I had to do the same thing with [nom]; it’s the
rule allowing *postfix expressions* to be used as *function identifiers*; issue
[here](https://github.com/phaazon/glsl/issues/45)).

# Is it really a parser?

Then I started to play with those generated `Rule` types… wait, what?! *The* generated `Rule` type?
Ok so that was my first surprise: the grammar knows how the rules are (deeply) nested but [pest]
outputs a single `Rule` type with all the rules laid down as flat variants for that `Rule`! This was
at first very confusing for me as I thought I’d be visiting freely through the [AST] – because this
is what a grammar actually defines. Instead, you must ask your `Parser` type which rule you want
to match against. You get a `Pairs`, which is an iterator over `Pair`s.

A `Pair` defines a single token that is defined by its starting point and ending point (hence a
pair… yeah I know, it’s confusing). A `Pair` has some methods you can call on it, like `as_str()` to
retrieve its string slice in the (borrowed) input of your parser. Or `into_inner()`, giving you a
`Pairs` that represent the matched inner tokens, allowing you to eventually visit through the tree
of tokens.

I have two problems with that.

## My unreachable goals

First, because [pest] doesn’t fully use the grammar information *at type-level*, you lose the one
crucial information: the actual topology of your grammar in the type system. That means that every
time you will ask for a given `Pair`, you will have to call `as_rule()` on it an pattern-match on
**the whole list of variants that compose your grammar!** In all situations, you will only need to
match against a very few of them and will discard every others. And how do you discard them? –
you *know* those are impossible because the very definition of the grammar? You use
`unreachable!()`, which I find almost as unsafe and ugly as using `unimplemented!()`. I know
`unreachable!()` is required in certain situations, but having it spawn in a parser using the public
API of [pest] makes me gnash my teeth. Example of code I’m currently writing:

```
fn parse_type_specifier_non_array<'a>(pair: Pair<'a>) -> Result<syntax::TypeSpecifierNonArray, String> {
  let inner = pair.into_inner().next().unwrap();

  match inner.as_rule() {
    Rule::struct_specifier => {
      // …
    }

    Rule::identifier => {
      let identifier = inner.as_str();

      match identifier {
        "void" => Ok(syntax::TypeSpecifierNonArray::Void),
        "float" => Ok(syntax::TypeSpecifierNonArray::Float),
        "double" => Ok(syntax::TypeSpecifierNonArray::Double),
        // …
        _ => Ok(syntax::TypeSpecifierNonArary::TypeName(identifier.to_owned())
      }
    }

    _ => unreachable!()
  }
}
```

And I reckon I will find that pattern a lot in my next few hours / days of writing those functions.

## Lexing and parsing are two different concepts

That `parse_type_specifier_non_array` function of mine also pinpointed another issue I had: initially,
I wrote that logic in the `type_specifier_non_array` rule in the PEG grammar, like:

```
type_specifier_non_array = {
  "void" |
  "float" |
  // …
  identifier
}
```

And quickly came to the realization that the `identifier` alternative would get *muted* very often
by the lexemes alternatives. Imagine two strings, `"int x = 3;"` and `"interop x = …;"`. The first
type will get the right `TypeSpecifierNonArray::Int` AST representation and the second string… will
get the same because the `"int"` rule will fire first, leaving some unmatched text without any
sense (`"erop x = …;"`).

What that means is that we cannot express this rule this way with PEG. I’ve been told [pest] should
now support the `!` operator that can be used to encode something that *must not be parsed*. But
that would just slightly hide the problem: there is no way in the PEG rules to actually do parsing
transformation. What [pest] really is here is a *lexer*: it generates tokens. Yes, it does place
them in a tree (token tree), but they all remains tokens (strings). And that is when I thought about
the difference between [pest] and [nom]

# Pest, nom… let’s explain

[nom] is a parser combinator. That means you can build bigger parsers by combining small parsers.
The correct term to talk about [nom] is that it’s a [scannerless parser]: it doesn’t require to
generate tokens prior to do parsing and prefers to do **both at the same time**. [nom] parsers are
typically implemented using macros like `preceded!`, `delimited!`, `take_until!`, `tag!`, `value!`
and `do_parse!`, allowing for matching (lexing) slices of bytes / chars *and* parsing them to actual
values with the type of your choice.

However, [pest] relies on a PEG file, representing the formal grammar of a language to tokenize.
That lexer phase takes place and has to be able to tokenize the whole input before giving hand back.
I’m not sure when I say this (but I’m pretty convince it’s the case): [pest] doesn’t support
streaming input, since it needs to eat the special rule `EOI` – End Of Input – or eat a rule error
(to succeed with the previous rule or propagate the error upwards) before returning. [nom] can be
used to eat streams of bytes, though.

And then, you have two choices with [pest]:

  1. Either you decide to expose the tokens as a token tree, in which case the AST contains string
     slices, completely left to be parsed.
  2. Either, like me, you have your own AST representation and you will have to write the actual
     parser over [pest].

I’m currently experimenting with (2.). The `type_specifier_non_array` rule is one of the biggest one
in my grammar so maybe I’ve already done the worst case but I’m fearing I’ll have long nights of
coding before ending up with a benchmark [pest] vs. [nom].

# Call to contributions

In the meantime:

  - I’ve written a very simple SPIR-V transpiler (that uses [shaderc] behind the hood). If you’re
    looking for an easy contribution, I’m looking for someone to test the feature branch holding
    the implementation, cleaning it, writing tests for it and merging it to `master`.
    [Issue here](https://github.com/phaazon/glsl/issues/21).
  - I’m looking for a way to encode `#define` and other preprocessors pragmas. I’m taking
    contributions (implementations, ideas) to fix that problem.
    [Issue here](https://github.com/phaazon/glsl/issues/39).
  - If you want to start working on refactoring the AST a bit (it was designed based on the Khronos
    grammar rules… which are quite ugly :P), please feel free to open an issue. I will eventually
    do it anyway sooner or later, but you know… :)
  - A lot of the types in the `syntax` module (the AST types) don’t have `new` methods. Those
    methods are important to me as they enable to bypass some limitation from the current
    `glsl-quasiquote` implementation that doesn’t allow variable interpolation.
  - Anything else! Feel free to contribute!

Thanks for having read through, and keep the vibes!

[glsl]: https://crates.io/crates/glsl
[nom]: https://crates.io/crates/nom
[pest]: https://crates.io/crates/pest
[parser combinator]: https://en.wikipedia.org/wiki/Parser_combinator
[\@geal]: https://github.com/geal
[PEG]: https://en.wikipedia.org/wiki/Parsing_expression_grammar
[Kleene star]: https://en.wikipedia.org/wiki/Kleene_star
[Kleene plus]: https://en.wikipedia.org/wiki/Kleene_star#Kleene_plus
[AST]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
[scannerless parser]: https://en.wikipedia.org/wiki/Scannerless_parsing
[shaderc]: https://crates.io/crates/shaderc
