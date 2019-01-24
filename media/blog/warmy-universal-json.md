# Universal support in warmy 0.11.1

I just added a new feature to [warmy] to make it more accessible and easy to use. It concerns [JSON]
and [serde].

If you’ve been using [warmy] a bit, you might be used to its traits and structures, like `Load`,
`Loaded`, `Load::reload`, `Res`, etc. All those concepts are mandatory to implement loading,
reloading and scarce resource sharing. Since version
[0.7](https://github.com/phaazon/warmy/blob/master/CHANGELOG.md#07), [warmy] got *loading and
reloading methods*. That feature enables you to have several `impl` for the same type of resource by
changing the method used to load. The default is `()` but you’re free to use any you want.

[warmy] 0.11.1 uses that

[warmy]: https://crates.io/crates/warmy/0.11.1
[JSON]: https://fr.wikipedia.org/wiki/JavaScript_Object_Notation
[serde]: https://crates.io/crates/serde
