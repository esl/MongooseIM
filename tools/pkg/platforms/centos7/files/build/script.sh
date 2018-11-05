#!/usr/bin/env bash
COMMIT=$1
REV=$2
set -e

HOSTNAME=`hostname`
ARCH="x86_64"

MONGOOSEIM_REPO=${MONGOOSEIM_REPO:-https://github.com/esl/mongooseim.git}

wget http://packages.erlang-solutions.com/erlang-solutions-1.0-1.noarch.rpm
sudo rpm -Uvh --replacepkgs erlang-solutions-1.0-1.noarch.rpm

git clone ${MONGOOSEIM_REPO} mongooseim
cd mongooseim
git checkout ${COMMIT}

VER=$(cat VERSION)


export ERL_TOP=`pwd`
sudo groupadd mongooseim
sudo adduser -g mongooseim mongooseim
mkdir -p rpmbuild/{RPMS,SOURCES,SPECS,SRPMS,BUILD,BUILDROOT/mongooseim-${VER}-${REV}.${ARCH}/}
chmod 777 -R rpmbuild
cp ../mongooseim.spec rpmbuild/SPECS/mongooseim.spec

sed -i "s#@ARCH@#${ARCH}#" rpmbuild/SPECS/mongooseim.spec

sudo yum-builddep rpmbuild/SPECS/mongooseim.spec -y
sudo yum erase rpm-build -y
sudo yum install rpm-build -y

cp ../rpmmacros.in            $HOME/.rpmmacros
sed -i "s#@ARCH@#${ARCH}#"    $HOME/.rpmmacros
sed -i "s#@VSN@#${VER}#"      $HOME/.rpmmacros
sed -i "s#@REVISION@#${REV}#" $HOME/.rpmmacros

./tools/configure with-all user=mongooseim prefix=/ system=yes
sed -i "s#PREFIX=\"/\"#PREFIX=\"rpmbuild/BUILDROOT/mongooseim-${VER}-${REV}.${ARCH}/\"#" configure.out
source configure.out
make install

rpmbuild -bb rpmbuild/SPECS/mongooseim.spec

mv rpmbuild/RPMS/${ARCH}/mongooseim-${VER}-${REV}.${ARCH}.rpm ../mongooseim-${VER}-${REV}~${HOSTNAME}.${ARCH}.rpm
