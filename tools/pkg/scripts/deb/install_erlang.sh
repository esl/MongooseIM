#!/usr/bin/env bash
set -euo pipefail

DISTRO=$(echo $dockerfile_platform | cut -f1 -d:)
ERLANG_PKG=esl-erlang_${erlang_version}-1~${dockerfile_platform/:/\~}_amd64.deb

curl -O https://binaries2.erlang-solutions.com/$DISTRO/pool/contrib/e/esl-erlang/$ERLANG_PKG
apt-get -y --no-install-recommends install ./$ERLANG_PKG

rm $ERLANG_PKG
