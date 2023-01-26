ARG OTP_VERSION="25.2"
FROM erlang:$OTP_VERSION AS builder

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
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Install rebar
ARG REBAR3_VERSION="3.17.0"
RUN set -xe \
    && REBAR3_DOWNLOAD_URL="https://github.com/erlang/rebar3/releases/download/${REBAR3_VERSION}/rebar3" \
    && curl -SL -o rebar3 "$REBAR3_DOWNLOAD_URL" \
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
VOLUME ["/member", "/var/lib/mongooseim"]

EXPOSE 4369 5222 5269 5280 9100

# Allow an easy access to mongooseim and mongooseimctl commands
ENV PATH="${PATH}:/usr/lib/mongooseim/bin"

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

COPY --from=builder /mim/_build/prod/rel/mongooseim /usr/lib/mongooseim
COPY ./tools/docker/start.sh /start.sh

CMD ["/start.sh"]
