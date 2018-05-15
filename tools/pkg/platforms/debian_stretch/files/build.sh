#!/bin/bash

if [ $# != 2 ]; then
    echo "Usage $0 <github_tag> <package_revision>"
    exit 1
fi
cd /build
bash script.sh $@
mv /build/mongooseim*deb /packages
