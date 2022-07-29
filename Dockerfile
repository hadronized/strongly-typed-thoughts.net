FROM archlinux:latest

# Dependencies.
RUN pacman -Syu --noconfirm
RUN pacman -S file --noconfirm

# Binary.
RUN mkdir /usr/local/bin/phaazon.net
ADD ./webserver/target/release/webserver /usr/local/bin/phaazon.net

# Read-only files.
RUN mkdir -p /usr/share/phaazon.net/
ADD ./static /usr/share/phaazon.net/static
ADD ./server.toml /usr/share/phaazon.net

# Data (uploads and blog articles).
RUN mkdir -p /var/lib/phaazon.net/blog
RUN mkdir -p /var/lib/phaazon.net/uploads

ENV PHAAZON_NET_CONFIG=/usr/share/phaazon.net/server.toml
ENTRYPOINT /usr/local/bin/phaazon.net/webserver
