# phaazon.net website

This repository holds the code source of https://phaazon.net.

![](https://phaazon.net/media/uploads/bonjourbonjour.gif)

It is written in Rust with [rocket-rs](https://rocket.rs/). It also holds my blog.

## Design

It’s pretty simple: routes are defined in `routes/` and are cached using [`Cache`](./webserver/src/cache.rs). That cache
has a TTL that is set to a low value for debug builds (development version) and around one day in production. The cache
is used for HTML pages, blog content and the RSS feed.

Everything runs in a `docker` in a dedicated machine I pay with my little euros.
