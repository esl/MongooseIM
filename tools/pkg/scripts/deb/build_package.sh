#!/usr/bin/env bash
set -e

version=$1
revision=$2
otp_version=$3

arch=$(dpkg --print-architecture)

cd ~/mongooseim

deluser --remove-home mongooseim --quiet || true
adduser --quiet --system --shell /bin/sh --group mongooseim

apt-get update

rm -rf /usr/lib/erlang/man/man3/cerff.3.gz /usr/lib/erlang/man/man3/cerfl.3.gz /usr/lib/erlang/man/man3/cerfcl.3.gz /usr/lib/erlang/man/man3/cerfcf.3.gz /usr/lib/erlang/man/man3/cerfcf.3.gz /usr/lib/erlang/man/man1/x86_64-linux-gnu-gcov-tool.1.gz  /usr/lib/erlang/man/man1/ocamlbuild.native.1.gz  /usr/lib/erlang/man/man1/gcov-tool.1.gz /usr/lib/erlang/man/man1/ocamlbuild.byte.1.gz

OS_ID=$(grep ^ID= /etc/os-release | cut -d= -f2 | tr -d '"')
OS_VERSION=$(grep ^VERSION_ID= /etc/os-release | cut -d= -f2 | tr -d '"' | cut -d. -f1)
if { [ "$OS_ID" = "ubuntu" ] && [ "$OS_VERSION" -lt 22 ]; } || \
   { [ "$OS_ID" = "debian" ] && [ "$OS_VERSION" -lt 12 ]; }; then
    export CFLAGS="-I/usr/local/ssl/include"

    # Copy essential OpenSSL 3.x runtime files to bundle with the app
    mkdir -p mongooseim/opt/mongooseim/openssl/etc
    mkdir -p mongooseim/opt/mongooseim/openssl/include
    cp -a /usr/local/ssl/include/openssl mongooseim/opt/mongooseim/openssl/include/

    if [ -d /usr/local/ssl/lib64 ]; then
        export LDFLAGS="-L/usr/local/ssl/lib64"
        cp -a /usr/local/ssl/lib64 mongooseim/opt/mongooseim/openssl/
    else
        export LDFLAGS="-L/usr/local/ssl/lib"
        cp -a /usr/local/ssl/lib mongooseim/opt/mongooseim/openssl/
    fi
fi

make clean
sed -i '1 s/^.*$/\#\!\/bin\/bash/' tools/install
./tools/configure user=mongooseim prefix="" system=yes
sed -i 's#PREFIX=""#PREFIX="mongooseim"#' configure.out
source configure.out
export GIT_SSL_NO_VERIFY=1

make install
cp -r ../deb/debian mongooseim/DEBIAN
mkdir -p mongooseim/etc/systemd/system/
cp -r ../deb/mongooseim.service mongooseim/etc/systemd/system/


sed -i "s#@ARCH@#${arch}#" mongooseim/DEBIAN/control
sed -i "s#@VER@#${version}-${revision}#" mongooseim/DEBIAN/control
sed -i "s#@VER@#${version}-${revision}#" mongooseim/DEBIAN/changelog

# set date in the dummy changelog
date=$(date -R)
sed -i "s#@DATE@#${date}#g" mongooseim/DEBIAN/changelog

chown $USER:$USER -R mongooseim
dpkg --build mongooseim ./

source /etc/os-release
os=$ID
os_version=$VERSION_CODENAME
package_os_file_name=${os}~${os_version}
mv mongooseim_*.deb ~/mongooseim_${version}_${revision}_otp_${otp_version}~${package_os_file_name}_${arch}.deb

