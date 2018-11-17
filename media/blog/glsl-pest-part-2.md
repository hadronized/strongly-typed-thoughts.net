This is the second article about my experience at supporting [pest] in my [glsl] crate – without,
for now, removing the [nom] parser.

> You can read the first article
> [here](https://phaazon.net/https://phaazon.net/blog/glsl-pest-part-1).

# RY: DRY without D

I feel like I’m either doing something wrong… or that something is wrong with using [pest] to build
typed [AST]s. I spent a whole day writing the GLSL450 [PEG]. After having the compiler stop failing
with left-recursion errors, I was finally able to test my crate!… or was I?

First thing first, I decided to write tests to see whether I can recognize *identifiers*. I’m a
TDD enthusiast – don’t get the idea twisted: writing tests is boring; but writing tests will save
your life and prevent small and cute kittens joining a project from drowning (true story).

I have a PEG rule for that, `identifier`. So the idea is the following:

```
// we set the error type to String for that first version
fn parse_identifier<'a, S>(input: S) -> Result<syntax::Identifier, String>
where S: Into<&'a str> {
  // here we get a set of pairs that match the identifier
  let pairs = Parser::parse(Rule::identifier, input.into()).map_err(|e| format!("{}", e))?;

  // since only a full string identifier would match, we can just simply ask to retrieve the string
  // that composes the pair
  Ok(pairs.as_str().to_owned())
}
```

As you can see, we have lost the grammar information that states that the `identifier` rule outputs
a single string – see in the code above how we get a `Pairs` object, assigned to `pairs`. This is
my first concern: the `Rule` type should contain information about what kind of values it was
matched against.

A more drastic example is the `primary_expression` rule, that defines expressions that can be either
an `identifier`, a `float_constant`, `int_constant`, `bool_constant` or any general expression with
parenthesis around. Its parser code would look like this:

```
fn parse_primary_expression<'a, S>(input: S) -> Result<syntax::Expr, String>
where S: Into<&'a str> {
  let pairs = Parser::parse(Rule::primary_expression, input.into()).map_err(|e| format!("{}", e))?;

  // we know we only have one expression, so this statement seems a bit out of nowhere here
  let pair = pairs.next().unwrap();

  // now, we know the primary expression is correctly tokenized, so we’re interested in “what’s
  // inside”; here, we HAVE to look at the grammar again to check what are the possible variants;
  // only one possible, let’s take the next pair then as well…
  let inner = pair.into_inner().next().unwrap();

  // here the pair represents the “real” and “useful” sub-rule that was matched; in order to write
  // that match block, we also have to look at the grammar to see all the possible variants
  match inner.as_rule() {
    Rule::identifier => {
      // the pair represents an identifier… we can just turn it to string and return it
      Ok(syntax::Expr::Variable(inner.as_str().to_owned())) // (1.)
    }

    Rule::float_constant => {
      // the pair represents a float constant; let’s just parse it
      let f = inner.as_str().parse().unwrap();
      Ok(syntax::Expr::FloatConstant(f))
    }

    Rule::etcetc. => {
      // etc. etc.
    }

    _ => unreachable!() // eeew (2.)
  }
}
```

As you can see, we have several problems here

  1. We need to write our parsers taking as input `Pair` so that we can compose them… that strangely
     resembles how the [nom] parser is actually written… :)
  2. We just repeat the actual grammar! The whole PEG file is just a way to tell our lexer ([pest])
     how to recognize tokens and how to store them in a tree. That’s all. All the parsing analysis
     must be done by *repeating ourselves*.
  3. Whenever we decide to change the grammar by, for instance, modifying the content of a rule
     (without changing its name), we’re lost. Done. Terminus. *Destination fucked*. Destination
     sneakily stabbed in the back. The code will still compile but now you will get a runtime error
     linked to `Result::unwrap`, a `Parse::parse` failure, etc.

Maybe I’m plain wrong and that there’s an easier way to do that, but I’ve been writing the parsers
of only a few rules (on more than fifty) and I already cringe.

# Two-stage parsers

All of this brings a brand new problem: since we’re smart developers and want to write the most
reusable code, we want to be able to write the parser of `primary_expression` once and reuse it in
other parsers that might need it – like `postfix_expression`, `expression`, etc. The current code
that consumes `Into<&str>` doesn’t allow this as [pest] parses to `Pairs`. So let’s just write our
functions to take `Pair` as inputs!

But… now we don’t have a proper public facing interface for our crate. Surely, I don’t want people
to even see that [pest] is used – they’ll just see it as a dependency, but I don’t want any [pest]
symbols in my public interface.

That problem can be solved by introducing a trait, `Parse`, for instance, that has the right 
parsing signature:

```
/// Class of types that can be parsed.
pub trait Parse: Sized {
  /// Parse an item from a string.
  ///
  /// Store errors as strings for convenience for now.
  fn parse<'a, S>(input: S) -> Result<Self, String> where S: Into<&'a str>;
}
```

Then, to support a new syntax symbol, we must:

  1. Write a `Pair` parser.
  2. Implement our `Parse` trait for this type, using the `Pair` parser from just above.
  3. Eventually reuse the `Pair` parser in the implementations of other `Pair` parsers if
     needed.

# Is it the right way to go?

I must say: I’m astonished by the complexity and verbosity – and dangerosity! – of all of this. This
is “on me” because I’ve constrained myself here:

  - I don’t want [pest] to leak any symbol into my public interface.
  - I want to be able to reuse parsers for a maximum of coherency between parsers – and more
    readable parsing code base.
  - I want my final output to be folded into an AST type of mine. More specifically, I want users of
    the [glsl] crate to be able to `parse` any type from the grammar / syntax. This is currently the
    case with the [nom] parser, that uses a similar trait. To implement the trait, I just defined a
    very small `macro_rules` that simply invokes the required parser for the given type.
    [Code here](https://github.com/phaazon/glsl/blob/a31b701850dd7966b14cc59f10c8d455c755c9c6/src/parser.rs#L99).

My main problem here is that there’s no way to have [pest] behave as a [scannerless parser]: you
will always have this two-phases lexer-parser:

  1. Run the lexer to tokenize the input string into a tree of tokens.
  2. Run a (type-blind) parser that somehow *rediscovers* the token tree.

I would be okay with this, however:

  1. Run the lexer to tokenize the input string into a *typed* tree of tokens.
  2. Run a full-type-aware parser that visits the token tree by following typed rules.
  3. It would be even easier if I could write the parser code *inline*, directly in the PEG rule.

Once compiled, a [pest] grammar is basically a very big and flat `Rule` type. If you have this PEG:

```
WHITESPACE = _{ " " | NEWLINE }

number = { ASCII_DIGIT+ }
op = { "-" | "+" | "*" }
expr = {
  number ~ (op ~ expr)? |
  "(" ~ expr ~ ")"
}
```

A possible AST – one I would definitely write – for that would be:

```
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Op {
  Minus,
  Plus,
  Mul
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum Expr {
  Number(i32),
  Op(Number, Op, Box<Expr>)
}
```

However, [pest] outputs something like this:

```
enum Rule {
  WHITESPACE, // very useful, thank you!
  number,
  op,
  expr
}
```

And implements the `Parser` trait to yield `Pairs` according to the variant of `Rule` you choose to
parse with. Instead, a more practical encoding of rules would be:

```
struct Rule_number;
impl SuperMegaParser<i32> for Rule_number {}

struct Rule_op;
impl SuperMegaParser<Op> for Rule_op {}

struct Rule_expr;
impl SuperMegaParser<Expr> for Rule_expr {}
```

That is, `Parser::parse(Rule_expr)` wouldn’t yield `Pairs` anymore, but `Expr`. In order for this
to be possible, we would need to tell our [scannerless parser] what to do when matching a rule:

```
WHITESPACE = _{ " " | NEWLINE } // who cares about whitespaces, seriously? they’re MUTED! :D

number: i32 = { ASCII_DIGIT+ => |s| s.parse().unwrap() }

op: Op = {
  "-" => Op::Minus |
  "+" => Op::Plus |
  "*" => Op::Mul
}

expr: Expr = {
  number ~ (op ~ expr)? => |(n, opt_op_expr)|{
    match opt_op_expr {
      Some((op, expr)) => Expr::Op(n, op, Box::new(expr)),
      None => Expr::Number(Number(n))
    }
  }|

  "(" ~ expr ~ ")"
}
```

This would be perfect to me. And I reckon it’s pretty much what [lalrpop] uses.

# Conclusion

I’ve been feeling on and off about [pest] lately, to be very honest. At first I was amazed at the
PEG file, because, yeah, PEG is lovely to work with. However, I think GLSL450 is a really good
candidate to test a lexer / parser and to that matter, the current vanilla [pest] is a nah to me.
It makes the code terribly bloated and harder to maintain – while it should be easier! – than the
reference [nom] implementation. The very reason to that is that even if I had to write thousands
line of macros calls – yiiiik – with [nom], those macros are correctly typed. The `identifier` [nom]
parser is a function taking bytes and outputing… `syntax::Identifier`. Same thing for all other
parsers.

I had to try [pest]. From my – limited – experience of it, I’d say **it’s definitely not a
parser**. [nom] is – a [scannerless parser]. [pest] **is a lexer** that does a *few parsing* work
to sort and fold the lexemes (tokens) into a tree. You can see [pest]’s output as a big regular
acyclic tree holding pairs of pointers (representing tokens in the input source). That’s everything
[pest] gives you. In order to turn that representation into a typed AST, a lot of work is awaiting
you. I’ll take a few hours / days to think about what I should do and work on other projects in the
meantime. I doubt I’ll keep going with [pest] because I feel like I’m going to spend entire days
repeating myself by looking the grammar up to, frustrated, shout on IRC that I’m writing untyped
code while I have static assurances (i.e. remember: the grammar is read at compile-time) that I will
never have to look at the `Rule::external_declaration` while parsing a `Rule::postfix_expression`.
And as I might be changing the grammar rule a bit when refactoring / fixing bugs, I really really
don’t plan to pay a visit to *destination fucked* yet.

> Someone on reddit suggested me to have a look at [pest-ast](https://github.com/pest-parser/ast).
> Even if this looks promising, it doesn’t seem finished and I think it should be merged into [pest]
> directly when done.

That’s all for me today. I might try a bit further to see if I find a way to make my [pest]
experience less painful but if I don’t, I might try that [lalrpop] cutie in a few hours / days! As
a final note, I don’t want to state that [pest] is bad, I think it’s pretty cool and that it does
its job greatly, but my need (typed AST) might be off its scope. That’s all to note, I’d say. :)

Keep the vibes.

[glsl]: https://crates.io/crates/glsl
[nom]: https://crates.io/crates/nom
[pest]: https://crates.io/crates/pest
[AST]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
[PEG]: https://en.wikipedia.org/wiki/Parsing_expression_grammar
[scannerless parser]: https://en.wikipedia.org/wiki/Scannerless_parsing
[lalrpop]: https://github.com/lalrpop/lalrpop
