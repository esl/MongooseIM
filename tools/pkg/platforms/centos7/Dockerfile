# Build container for Centos 7 RPM packages
FROM centos:7
MAINTAINER Radek Szymczyszyn <radoslaw.szymczyszyn@erlang-solutions.com>

# Install the build dependencies
RUN yum install -y wget
RUN wget http://mirror.centos.org/centos/7/extras/x86_64/Packages/epel-release-7-9.noarch.rpm
RUN rpm -ivh epel-release-7-9.noarch.rpm
RUN wget http://packages.erlang-solutions.com/erlang-solutions-1.0-1.noarch.rpm
RUN rpm -Uvh erlang-solutions-1.0-1.noarch.rpm
RUN yum --setopt=obsoletes=0 install -y sudo telnet lsof vim gcc rpm-build rpm-sign make automake expat-devel \
                   git gcc-c++ kernel-devel openssl openssl-devel yum-utils chrpath unixODBC-devel esl-erlang-19.3.6-1 \
 && yum clean all
# Fix locale setup once packages are installed.
# See https://github.com/CentOS/sig-cloud-instance-images/issues/71#issuecomment-266957519
RUN localedef -i en_US -f UTF-8 en_US.UTF-8

# Package output mountpoint
VOLUME /packages

# Copy the build script
COPY platforms/centos7/files/build /buildfiles
COPY files/build /build

ENTRYPOINT ["/build"]
