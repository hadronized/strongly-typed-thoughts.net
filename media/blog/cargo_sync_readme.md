Have you ever struggled with welcoming people on your project pages, be them on [GitHub] or
[docs.rs]? One major problem we’re facing when developping crates is that we need to maintain
several places where documentation is important:

  - The `lib.rs` / `main.rs` files, that get rendered as front page on [docs.rs]. This is important
    because we want people to have nice onboardings when hitting the official documentations of our
    crates.
  - The README file — often `README.md`, that gets automatically rendered as HTML when you end up on
    the project’s [GitHub] page. This page should provide as many as possible hints and information
    about what the project is and how to use it.

However, sometimes, I just don’t want to bother writing twice the same thing and honestly, I don’t
really know whether the README file is a good place to write an onboarding section, since that
should also be included in your Rust documentation on [docs.rs].

So… I’ve been thinking of a way to fix that, without really investing too much time in it. But
lately, I came to the realization that I often have this pattern:

  1. Write the onboarding documentation in my `lib.rs` or `main.rs`. For instance, I’m pretty proud
     of the [onboarding documentation of warmy]. It’s exhaustive, easy for newcomers, it has plenty
     of links and explanations and it renders pretty nice on [docs.rs].
  2. Write a header in the README file with badges etc. and then copy the Rust onboarding
     documentation in the readme.

Here, (2.) is a manual and annoying task: I open my editor, make a block selection of the
documentation, store it in a buffer, apply a smart macro on it to remove the Rust annotation and
then paste the result in the README after having purged it from the previous documentation… That’s
tedious and not very interesting.

# `cargo sync-readme` to the rescue!

So, yesterday, I decided to start working on a small tool that would automate all this for me. The
idea is the following:

  - `cargo sync-readme` synchronizes your README (the file specified by the `readme` key in your
    `Cargo.toml`, or just `README.md` by default) with the entrypoint of your library or binary
    crate (by default, `lib.rs` or `main.rs`, or what is defined at the `path` key in your
    manifest).
  - In order to perform the synchronization, you need to have put a special marker so that the
    command is instructed where to put the synchronized documentation.
      - The `<!-- cargo-sync-readme -->` marker must lie on a single line where you want the
        documentation to be inserted in your README.
      - Post generation, this marker will disappear and will get replaced by the synchronized
        documentation, surrounded by two markers: `<!-- cargo-sync-readme start>` and
        `<!-- cargo-sync-readme end -->`.

This is really all you have to do. `cargo sync-readme` will take care of the rest for you. There’re
two hypothesis that the command requires to be true, though:

  - Your README file should be a Markdown-formatted file. It doesn’t have to be, but since Rust’s
    documentation is written in Markdown, it should be.
  - You use the `//!` annotation to write your documentation. This is currently how
    `cargo sync-readme` works. It doesn’t have to be solely using this annotation on longer term,
    but currently, this is the only annotation supported. More can be added if needed.

Basically, insert the marker once, and run `cargo sync-readme`. Every time you change your Rust
documentation, just call `cargo sync-readme` once again to synchronize the documentation in your
README file.

## On workspace crates

Currently, `cargo sync-readme` doesn’t work with workspace crates. You will have to go into each of
the workspace’s members to run `cargo sync-readme` if you want to synchronize their respective
READMEs.

# Conclusion

`cargo sync-readme` is already available on [crates.io] for you to use. You can install it as a
development tool with the following command:

```
cargo install cargo-sync-readme
```

> Disclaimer: after having published `cargo-sync-readme`, I was told that there is already another
> cargo plugin, [cargo-readme], that already does that. Indeed, that crate does more or less the
> same job. However, the way it does it is very different. First, it uses a template file while
> `cargo-sync-readme` lets you use your README file without having to depend on a template. Also,
> [cargo-readme] has special semantics in its template (like {{crate_name}}, etc.) while
> `cargo-sync-readme` is simpler and just requires a single marker. To sum up: `cargo-readme` is
> heavier and is likely to require you to break your file into two separate files but contains more
> customization options while `cargo-sync-readme` only requires a single line in your README and
> will synchronize from within that same file.

Feel free to comment, open issues, drink beers, share that [awesome bear driving a sausage podracer]
and most of all… keep the vibes!

  - [`cargo-sync-readme` on GitHub](https://github.com/phaazon/cargo-sync-readme)
  - [`cargo-sync-readme` on crates.io](https://crates.io/crates/cargo-sync-readme)

[GitHub]: https://github.com
[docs.rs]: https://docs.rs
[onboard documentation of warmy]: https://docs.rs/warmy
[crates.io]: https://crates.io/crates/cargo-sync-readme
[cargo-readme]: https://crates.io/crates/cargo-readme
[awesome bear driving a sausage podracer]: https://phaazon.net/media/uploads/bear_sausage.mp4
