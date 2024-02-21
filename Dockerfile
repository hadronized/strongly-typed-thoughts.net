FROM archlinux:base

# Dependencies.
RUN pacman -Syu --noconfirm
RUN pacman -S file --noconfirm

# Binary.
RUN mkdir /usr/local/bin/strongly-typed-thoughts.net
ADD ./webserver/target/release/webserver /usr/local/bin/strongly-typed-thoughts.net

# Read-only files.
RUN mkdir -p /usr/share/strongly-typed-thoughts.net/
ADD ./static /usr/share/strongly-typed-thoughts.net/static
ADD ./server.toml /usr/share/strongly-typed-thoughts.net

# Data (uploads and blog articles).
RUN mkdir -p /opt/strongly-typed-thoughts.net/blog
RUN mkdir -p /opt/strongly-typed-thoughts.net/uploads

ENV WEBSERVER_CONFIG=/usr/share/strongly-typed-thoughts.net/server.toml
ENTRYPOINT /usr/local/bin/strongly-typed-thoughts.net/webserver
