#!/usr/bin/env bash
set -euo pipefail

DISTRO=$(echo $dockerfile_platform | cut -f1 -d:)
OS_RELEASE=$(echo $dockerfile_platform | cut -f2 -d:)

# There is no Erlang package for Almalinux, so let's use the one for Rockylinux
if [ $DISTRO == almalinux ]; then DISTRO=rockylinux; fi

ERLANG_PKG=esl-erlang_${erlang_version}_1~${DISTRO}~${OS_RELEASE}_x86_64.rpm

curl -O https://binaries2.erlang-solutions.com/$DISTRO/$OS_RELEASE/$ERLANG_PKG
yum install -y ./$ERLANG_PKG

rm $ERLANG_PKG
