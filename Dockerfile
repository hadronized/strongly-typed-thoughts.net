FROM ubuntu:latest

# Binary.
RUN mkdir /usr/local/bin/phaazon.net
ADD target/release/webserver /usr/local/bin/phaazon.net

# Read-only frontend and static files.
RUN mkdir -p /usr/share/phaazon.net/
ADD ./frontend /usr/share/phaazon.net
ADD ./static /usr/share/phaazon.net

# Data (uploads and blog articles).
# RUN mkdir -p /var/lib/phaazon.net/uploads/

ENTRYPOINT /usr/bin/webserver
