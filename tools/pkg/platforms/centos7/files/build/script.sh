#!/usr/bin/env bash
set -e

git_ref=$1
revision=$2
repo_url=$3
specfile_down_erl_vsn=$4
specfile_up_erl_vsn=$5

arch="x86_64"
package_name_arch="amd64"

git clone "$repo_url" mongooseim
cd mongooseim
git checkout "$git_ref"

version=$(cat VERSION)
commit_sha=$(git rev-parse --short HEAD)

# Adjust package revision to requirements:
# https://twiki.cern.ch/twiki/bin/view/Main/RPMAndDebVersioning
if git show-ref --verify "refs/tags/$git_ref" &>/dev/null && $version == "$git_ref"; then
    package_revision="${revision}"
elif git show-ref --verify "refs/heads/$git_ref" &>/dev/null; then
    package_revision="${revision}.${git_ref}.${commit_sha}"
else
    package_revision="${revision}.${commit_sha}"
fi
package_version="${version}-${package_revision}"

cd ..

# below commands are adjusted to defaults of rpmdev-setuptree and rpmbuild commands
rpmdev-setuptree
cp ./mongooseim.spec ~/rpmbuild/SPECS/.
cp ./mongooseim.service ~/rpmbuild/SOURCES/mongooseim.service

cp -r ./mongooseim ~/rpmbuild/BUILD/mongooseim-$version

rpmbuild -bb \
    --define "version ${version}" \
    --define "release ${package_revision}" \
    --define "architecture ${arch}" \
    --define "esl_erlang_ver_up ${specfile_up_erl_vsn}" \
    --define "esl_erlang_ver_down ${specfile_down_erl_vsn}" \
    ~/rpmbuild/SPECS/mongooseim.spec

source /etc/os-release
os=$ID
os_version=$VERSION_ID
package_os_file_name=${os}~${os_version}

mv ~/rpmbuild/RPMS/$arch/*.rpm \
    "/root/rpm/mongooseim_${package_version}~${package_os_file_name}_${package_name_arch}.rpm"
