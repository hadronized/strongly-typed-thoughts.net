FROM ubuntu:latest

ADD target/release/webserver /usr/bin
ENTRYPOINT /usr/bin/webserver
