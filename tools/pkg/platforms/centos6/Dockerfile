# Build container for Centos 6 RPM packages
FROM centos:6
MAINTAINER Radek Szymczyszyn <radoslaw.szymczyszyn@erlang-solutions.com>

# Install the build dependencies
RUN yum install -y wget
RUN yum install -y centos-release-scl # install REPO with extra devtools - gcc-c++ with C++11
RUN wget http://mirror.centos.org/centos/6/extras/x86_64/Packages/epel-release-6-8.noarch.rpm
RUN rpm -ivh epel-release-6-8.noarch.rpm
RUN wget http://packages.erlang-solutions.com/erlang-solutions-1.0-1.noarch.rpm
RUN rpm -Uvh erlang-solutions-1.0-1.noarch.rpm
RUN yum --setopt=obsoletes=0 install -y sudo telnet lsof vim gcc rpm-build rpm-sign make automake expat-devel \
                   git devtoolset-4-gcc-c++ devtoolset-4-gcc kernel-devel openssl openssl-devel yum-utils chrpath unixODBC-devel esl-erlang-18.3-1 \
 && yum clean all
# Fix locale setup once packages are installed.
# See https://github.com/CentOS/sig-cloud-instance-images/issues/71#issuecomment-266957519
RUN localedef -i en_US -f UTF-8 en_US.UTF-8

# Package output mountpoint
VOLUME /packages

# Copy the build script
COPY platforms/centos6/files/build /buildfiles
COPY files/build /build

# Copy the shim for SCL
# (see https://access.redhat.com/documentation/en-us/red_hat_software_collections/1/html/packaging_guide/sect-Enabling_the_Software_Collection)
COPY platforms/centos6/files/scl-build /

ENTRYPOINT ["/scl-build"]
