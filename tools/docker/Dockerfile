FROM ubuntu:20.04 AS builder

ENV DEBIAN_FRONTEND=noninteractive

# required packages
RUN apt-get update && apt-get install -y \
    sudo \
    bash \
    build-essential \
    apt-transport-https \
    software-properties-common \
    git \
    libssl-dev \
    zlib1g-dev \
    unixodbc-dev \
    gnupg \
    curl && \
    rm -rf /var/lib/apt/lists/*

# Install erlang
ARG OTP_VERSION="24.1"
RUN curl http://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb -o esl_2.0_all.deb && \
    dpkg -i esl_2.0_all.deb && \
    apt-get update && \
    apt-get install -y esl-erlang=1:$OTP_VERSION-1 && \
    apt-get clean

# Install rebar
ARG REBAR3_VERSION="3.17.0"
RUN set -xe \
    && REBAR3_DOWNLOAD_URL="https://github.com/erlang/rebar3/releases/download/${REBAR3_VERSION}/rebar3" \
    && REBAR3_DOWNLOAD_SHA256="391f325e6adb38736ffeab15f6f26d39bd0145a97dbbb41aa2e5251785d9c181" \
    && curl -SL -o rebar3 "$REBAR3_DOWNLOAD_URL" \
    && echo "$REBAR3_DOWNLOAD_SHA256 rebar3" | sha256sum -c - \
    && install -v ./rebar3 /usr/local/bin/

WORKDIR /mim

# Cache dependencies
COPY rebar.config rebar.lock /mim/
RUN rebar3 compile

COPY . /mim
ARG CONFIGURE="with-all"
RUN ./tools/configure ${CONFIGURE}
RUN rebar3 compile
RUN make rel

## Final image
FROM phusion/baseimage:focal-1.0.0-amd64
COPY --from=builder /mim/_build/prod/rel/mongooseim /usr/lib/mongooseim
COPY ./tools/docker/start.sh /start.sh
VOLUME ["/member", "/var/lib/mongooseim"]

EXPOSE 4369 5222 5269 5280 9100

ARG BUILD_DATE
ARG VCS_REF
ARG VCS_REF_DESC
ARG VERSION
LABEL org.label-schema.name='MongooseIM' \
      org.label-schema.description='MongooseIM is a mobile messaging platform with focus on performance and scalability' \
      org.label-schema.url='https://www.erlang-solutions.com/products/mongooseim.html' \
      org.label-schema.vcs-url='https://github.com/esl/MongooseIM' \
      org.label-schema.vendor='Erlang Solutions' \
      org.label-schema.schema-version='1.0' \
      org.label-schema.build-date=$BUILD_DATE \
      org.label-schema.vcs-ref=$VCS_REF \
      org.label-schema.vcs-ref-desc=$VCS_REF_DESC \
      org.label-schema.version=$VERSION

RUN apt-get update && apt-get install -y \
        libssl1.1 \
        iproute2 \
        netcat \
        inetutils-ping \
        telnet \
        unixodbc \
        tdsodbc \
        odbc-postgresql && \
        apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

ENTRYPOINT ["/start.sh"]
