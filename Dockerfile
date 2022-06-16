FROM ubuntu:latest

RUN cargo build --release
WORKDIR target/release
RUN strip -sx target/release/webserver
ADD target/release/webserver /usr/bin
ENTRYPOINT /usr/bin/webserver
