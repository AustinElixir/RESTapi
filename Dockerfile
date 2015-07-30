FROM ubuntu:trusty

MAINTAINER royce miller <royce.h.miller@gmail.com>

RUN DEBIAN_FRONTEND=noninteractive \
    apt-get update -qq \
    && apt-get install -y \
       build-essential \
       wget \
       git

RUN wget https://s3.amazonaws.com/rebar3/rebar3 -O /usr/bin/rebar3 \
    && chmod +x /usr/bin/rebar3

RUN wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb -O erlang-solutions_1.0_all.deb \
    && dpkg -i erlang-solutions_1.0_all.deb \
    && rm erlang-solutions_1.0_all.deb

RUN apt-get update -y \
    && apt-get update -yq --fix-missing \
    && apt-get install -y \
       erlang

RUN cd /usr/src \
    && git clone https://github.com/AustinErlang/RESTapi

RUN cd /usr/src/RESTapi \
    && rebar3 update \
#    && rebar3 release \
#    && mkdir -p /var/rest_db -m 0777 \
#    && erl -pa _build/default/lib/*/ebin -sname rest -mnesia dir '"/var/rest_db"' -run rest_db initial_setup -s init stop

EXPOSE 8080
