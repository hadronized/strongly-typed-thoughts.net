This is a very short blog article about a problem I found lately with [cargo].

# The problem

Imagine you have a crate `A` and a crate `B` and you want to upload them to crates.io. There is a
context setup:

  - You’re working in a [cargo workspace](https://doc.rust-lang.org/book/ch14-03-cargo-workspaces.html).
  - `A` depends on `B` via `[dev-dependencies]` because you use it in your `examples/` directory,
    for instance.
  - `B` depends on `A` via `[dependencies]` because it implements some interface or such.
  - You would like to upload `A` first (because uploading `B` without having `A` would make `B`
    unable to compile).

The main problem is that currently, you cannot do _anything_ with such a setup. If you try to
upload `A` to crates.io, crates.io will reject the crate because `B` doesn’t exist. That might be
stunning, but yes, [cargo] also check that your `[dev-dependencies]` are correctly aligned with your
`A`’s `Cargo.toml`. Meh.

If you try to upload `B`, obviously, since `A` is not there, crates.io will complain.

What should we do?

# The (dirty) solution

First thing first, here’s [a discussion](https://github.com/rust-lang/cargo/issues/4242) that helped
me figure out how to solve that issue. Disclaimer, though: it’s neither elegant and satisfying.

The first thing to do is to edit `A`’s `Cargo.toml` in order to completely strip its
`[dev-dependencies]` section. Just remove it. All of it.

Then, and that’s the tricky part: do not `git commit` the change. At that point, you have removed
the dependency from `A` to `B`, which is **not mandatory for people getting the crate from
crates.io.** That’s the nasty part. You’re about to upload a crate which metadata are not aligned
with your repository. I don’t like it, but I don’t see any other way to do it — however, you’ll see
we can fix it afterwards.

Then, upload `A` to crates.io:

```
cd A
cargo publish --allow-dirty
```

The `--allow-dirty` switch is needed because [cargo] would complain about your working tree not
being clean otherwise.

Now, you have `A` on crates.io. Just go on with `B`: it can now be uploaded:

```
cd ../B
cargo publish
```

Now you have, on crates.io:

  - `A` in version `X.Y.Z`, without any `[dev-dependencies]`.
  - `B`, whichever version.

The final part of the trick is to _restore_ the sanity of `A`. Because, yes, I don’t like having
something on crates.io that is not exactly the same thing as my repository.

Edit `A`’s `Cargo.toml` or check it out:

```
cd ..
git checkout .
```

Now you are exactly in the same situation as before trying anything to solve the problem. Edit
`A`’s `Cargo.toml` and increment its patch version. In my case, I would get version `X.Y.(Z+1)`.
`git commit` your changes, `git push`. You’re ready to patch crates.io:

```
cd A
cargo publish
```

Now you have:

  - `A` in version `X.Y.(Z+1)`, with the right `[dev-dependencies]`.
  - `B`, whichever version.

# Conclusion

I know it’s not an ideal fix, but at least we end up on our (clean) feets. I really really think
the [cargo] team should fix that issue, though. In that case, `A` and `B` were actually [luminance]
and [luminance-derive]. I will make a long article very soon about them to introduce new versions,
new crates and complete graphics tutorials on how to have fun with [luminance].

I hope you liked that article and as always, _keep the vibes_.

[cargo]: https://doc.rust-lang.org/cargo
[luminance]: https://crates.io/crates/luminance
[luminance-derive]: https://crates.io/crates/luminance-derive
