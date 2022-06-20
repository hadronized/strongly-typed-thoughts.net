FROM archlinux:latest

# Dependencies.
RUN pacman -Syu
RUN pacman -S file

# Binary.
RUN mkdir /usr/local/bin/phaazon.net
ADD webserver/target/release/webserver /usr/local/bin/phaazon.net

# Read-only frontend and static files.
RUN mkdir -p /usr/share/phaazon.net/
ADD ./frontend /usr/share/phaazon.net
ADD ./static /usr/share/phaazon.net

# Data (uploads and blog articles).
# RUN mkdir -p /var/lib/phaazon.net/uploads/

ENTRYPOINT /usr/local/bin/phaazon.net/webserver
