#!/usr/bin/env bash
set -e

version=$1
revision=$2

arch="x86_64"
package_name_arch="amd64"


rpmbuild -bb \
    --define "version ${version}" \
    --define "release ${revision}" \
    --define "architecture ${arch}" \
    ~/rpmbuild/SPECS/mongooseim.spec

source /etc/os-release
os=$ID
os_version=$VERSION_ID
package_os_file_name=${os}~${os_version}

mv ~/rpmbuild/RPMS/${arch}/mongooseim-${version}-${revision}.${arch}.rpm \
    ~/mongooseim_${version}-${revision}~${package_os_file_name}_${package_name_arch}.rpm
