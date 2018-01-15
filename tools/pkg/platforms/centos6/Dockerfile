# Build container for Centos 6 RPM packages
FROM centos:6
MAINTAINER Radek Szymczyszyn <radoslaw.szymczyszyn@erlang-solutions.com>

# Install the build dependencies
RUN yum install -y wget
RUN wget http://packages.erlang-solutions.com/erlang-solutions-1.0-1.noarch.rpm
RUN rpm -Uvh erlang-solutions-1.0-1.noarch.rpm
RUN yum --setopt=obsoletes=0 install -y sudo telnet lsof vim gcc rpm-build make automake \
                                        expat-devel git gcc-c++ kernel-devel openssl-devel \
                                        yum-utils chrpath erlang-18.3-1.el6 \
 && yum clean all

# Clone esl/package-buildscripts
ENV BUILDSCRIPTS_REPO https://github.com/esl/package-buildscripts
ENV BUILDSCRIPTS_REF 8a44113fd8f68a2ea7c064f1d8ea9971db443f4c
RUN git clone BUILDSCRIPTS_REPO package-buildscripts \
 && cd package-buildscripts && git checkout $BUILDSCRIPTS_REF

# Deploy key for fetching private GitHub repositories
COPY files/necromancer-docker.id_rsa     /root/.ssh/id_rsa
COPY files/necromancer-docker.id_rsa.pub /root/.ssh/id_rsa.pub

# GPG key for signing the packages
COPY files/necromancer.gpg.key /root/gpg.key
COPY files/esl-sign-private.key /root/esl-sign-private.key
RUN gpg --import /root/gpg.key
RUN gpg --allow-secret-key-import --import /root/esl-sign-private.key

# Package output mountpoint
VOLUME /packages

# Copy the build script
COPY files/build /build

CMD ["/bin/bash"]
