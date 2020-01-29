# Build container for Debian Stretch .deb packages
FROM debian:stretch

# Install the esl repo
RUN apt-get update && \
    apt-get install -y wget gnupg
RUN wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && \
    dpkg -i erlang-solutions_1.0_all.deb

# Install build deps
RUN apt-get update
RUN apt-get install -y git \
                       sudo \
                       make \
                       g++ \
                       libssl-dev \
                       libexpat-dev \
                       zlib1g-dev \
                       locales \
                       unixodbc-dev \
                       esl-erlang=1:22.1.8-1

# fix locales
RUN locale-gen en_US.UTF-8

# Package output mountpoint
VOLUME /packages

# Copy the build script
COPY platforms/debian_stretch/files/build /buildfiles
COPY files/build /build

ENTRYPOINT ["/build"]
