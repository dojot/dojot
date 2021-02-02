FROM ubuntu:18.04

# Install luarocks
RUN apt-get update -y && apt-get install -y luarocks

# Install busted
RUN luarocks install busted

# Install plugin
WORKDIR /tmp
COPY ./plugins/pepkong /tmp
RUN luarocks make

CMD ["busted", "tests/unit_tests/tests"]




