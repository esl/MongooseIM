#!/usr/bin/env bash
#standard necromancer script setup
VER=$1
REV=$2
set -e

MONGOOSEIM_REPO=${MONGOOSEIM_REPO:-https://github.com/esl/mongooseim.git}

if [ ! -e mongooseim ]; then
	git clone --branch ${VER} --single-branch $MONGOOSEIM_REPO mongooseim
	pushd mongooseim
	git checkout ${VER}
	popd
fi
cd mongooseim

sudo /usr/sbin/deluser --remove-home mongooseim --quiet || true
sudo /usr/sbin/adduser --quiet --system --shell /bin/sh --group mongooseim

sudo rm -rf /usr/lib/erlang/man/man3/cerff.3.gz /usr/lib/erlang/man/man3/cerfl.3.gz /usr/lib/erlang/man/man3/cerfcl.3.gz /usr/lib/erlang/man/man3/cerfcf.3.gz /usr/lib/erlang/man/man3/cerfcf.3.gz /usr/lib/erlang/man/man1/x86_64-linux-gnu-gcov-tool.1.gz  /usr/lib/erlang/man/man1/ocamlbuild.native.1.gz  /usr/lib/erlang/man/man1/gcov-tool.1.gz /usr/lib/erlang/man/man1/ocamlbuild.byte.1.gz

sed -i '1 s/^.*$/\#\!\/bin\/bash/' tools/install
./tools/configure with-all user=mongooseim prefix="" system=yes
sed -i 's#PREFIX=""#PREFIX="mongooseim"#' configure.out
source configure.out
export GIT_SSL_NO_VERIFY=1
sudo -E make install
sudo cp -r ../debian mongooseim/DEBIAN
sudo mkdir -p mongooseim/etc/systemd/system/
sudo cp -r ../mongooseim.service mongooseim/etc/systemd/system/

ARCH="amd64"
sudo sed -i "s#@ARCH@#${ARCH}#" mongooseim/DEBIAN/control
sudo sed -i "s#@VER@#${VER}-${REV}#" mongooseim/DEBIAN/changelog
dpkg --build mongooseim ./

mv mongooseim_*.deb ../mongooseim_${VER}-${REV}~${HOSTNAME}_${ARCH}.deb

