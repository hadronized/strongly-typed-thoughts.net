FROM ubuntu:latest

ADD target/release/webserver /usr/local/bin

RUN mkdir -p /usr/var/phaazon.net/media/

RUN mkdir -p /usr/local/phaazon.net/
ADD ./frontend /usr/local/phaazon.net
ADD ./static /usr/local/phaazon.net

ENTRYPOINT /usr/bin/webserver
