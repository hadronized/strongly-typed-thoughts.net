FROM ubuntu:latest

RUN strip -sx target/release/webserver
ADD target/release/webserver /usr/bin
ENTRYPOINT /usr/bin/webserver
