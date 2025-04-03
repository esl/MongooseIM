#!/usr/bin/env bash
set -e

version=$1
revision=$2
otp_version=$3

arch=$(uname -m)

case "$arch" in
  x86_64)
    package_name_arch="amd64"
    ;;
  aarch64)
    package_name_arch="arm64"
    ;;
  *)
    echo "Unsupported architecture: $arch"
    exit 1
    ;;
esac

OS_ID=$(grep ^ID= /etc/os-release | cut -d= -f2 | tr -d '"')
OS_VERSION=$(grep ^VERSION_ID= /etc/os-release | cut -d= -f2 | tr -d '"' | cut -d. -f1)
if { [ "$OS_ID" = "rocky" ] && [ "$OS_VERSION" -lt 9 ]; } || \
   { [ "$OS_ID" = "almalinux" ] && [ "$OS_VERSION" -lt 9 ]; }; then
    if [ -d /usr/local/ssl/lib64 ]; then
        export LDFLAGS="-L/usr/local/ssl/lib64"
    else
        export LDFLAGS="-L/usr/local/ssl/lib"
    fi
    export CFLAGS="-I/usr/local/ssl/include"

    bundle_openssl=1
else
    bundle_openssl=0
fi

rpmbuild -bb \
    --define "version ${version}" \
    --define "release ${revision}" \
    --define "architecture ${arch}" \
    --define "with_bundled_openssl ${bundle_openssl}" \
    ~/rpmbuild/SPECS/mongooseim.spec

source /etc/os-release
os=$ID
os_version=$VERSION_ID
package_os_file_name=${os}~${os_version}

mv ~/rpmbuild/RPMS/${arch}/mongooseim-${version}-${revision}.${arch}.rpm \
    ~/rpmbuild/mongooseim_${version}_${revision}_otp_${otp_version}~${package_os_file_name}_${package_name_arch}.rpm
