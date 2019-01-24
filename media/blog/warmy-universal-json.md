# Norbert the hot-dog

I just added a new feature to [warmy] to make it more accessible and easy to use. It concerns [JSON]
and [serde].

## Universal JSON support in warmy 0.11.1

If you’ve been using [warmy] a bit, you might be used to its traits and structures, like `Load`,
`Loaded`, `Load::reload`, `Res`, etc. All those concepts are mandatory to implement loading,
reloading and scarce resource sharing. Since version
[0.7](https://github.com/phaazon/warmy/blob/master/CHANGELOG.md#07), [warmy] has got *loading and
reloading methods*. That feature enables you to have several `impl` for the same type of resource by
changing the method used to load. The default is `()` but you’re free to use any you want. The idea
is that you can call the `Store::get_by` or `Store::get_proxied_by` methods to specify which method
to use explicitly when loading and reloading a given resource.

[warmy] 0.11.1 uses that concept to provide a universal implementor for anything that implements the
[`Deserialize`] trait from [serde]. Basically, once your type implements [`Deserialize`], you can
load and hot-reload values of this type by using the [`Json`] type from [warmy].

> Universal JSON implementors are available only if the `"json"` feature is enabled in [warmy]. It’s
> the case by default. Feel free to use `default-features = false` if you don’t want it.

![](https://phaazon.net/media/uploads/very_nice.gif)

Here’s a short example of what it looks like to load and hot-reload a `Dog` using the universal JSON
feature:

```
use serde::Deserialize;
use warmy::{Res, SimpleKey, Store, StoreOpt};
use warmy::json::Json;
use std::thread::sleep;
use std::time::Duration;

#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
struct Dog {
  name: String,
  gender: Gender
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
enum Gender {
  Female,
  Male
}

fn main() {
  let mut store: Store<(), SimpleKey> = Store::new(StoreOpt::default()).unwrap();
  let ctx = &mut ();

  let resource: Result<Res<Dog>, _> = store.get_by(&SimpleKey::from_path("/dog.json"), ctx, Json);

  match resource {
    Ok(dog) => {
      loop {
        store.sync(ctx);

        println!("Dog is {} and is a {:?}", dog.borrow().name, dog.borrow().gender);
        sleep(Duration::from_millis(1000));
      }
    }

    Err(e) => eprintln!("{}", e)
  }
}
```

This feature should help people adopt the crate and use it without worrying too much about the
actual implementation of `Load`.

If you’re interested in adding other methods, like YAML, XML or whatever, feel free to open a PR on
GitHub! I’ll be very happy to accept it. More documentation about the feature can be found in the
[README] or the [online documentation].

Keep the vibes.

[warmy]: https://crates.io/crates/warmy/0.11.1
[JSON]: https://fr.wikipedia.org/wiki/JavaScript_Object_Notation
[serde]: https://crates.io/crates/serde
[`Deserialize`]: https://docs.rs/serde/1.0.85/serde/trait.Deserialize.html
[`Json`]: https://docs.rs/warmy/0.11.1/warmy/json/struct.Json.html
[README]: https://github.com/phaazon/warmy/tree/193af4bc6ee75699fea1ab2cf4ecff362b919899#universal-json-support
[online documentation]: https://docs.rs/warmy/0.11.1/warmy/index.html#universal-json-support
