#!/usr/bin/env sh

set -e

# Params - override for your env
SHA256SUM="shasum -a 256"
REMOTE_SHA256SUM=sha256sum
PACKAGES_LOCAL=packages
. env/publish

# CLI arguments
VERSION=${1:-"(missing)"}
REVISION=${2:-1}

if [ x"$VERSION" = x"(missing)" ]; then
    echo "usage: $0 VERSION [REVISION]"
    echo
    echo Look for package \$VERSION-\$REVISION and publish to $PACKAGES_HOST.
    exit 1
fi

set +e
PKG=$(ls -1 ${PACKAGES_LOCAL} | grep "$VERSION-$REVISION" | head -1)
if [ -z "$PKG" ]; then
    echo "$0: can't find package version $VERSION-$REVISION"
    exit 2
fi
set -e

rsync $PACKAGES_LOCAL/$PKG $PACKAGES_HOST:$PACKAGES_PATH
CHKSUM=$($SHA256SUM $PACKAGES_LOCAL/$PKG)
REMOTE_CHKSUM=$(ssh $PACKAGES_HOST "cd $PACKAGES_PATH && $REMOTE_SHA256SUM $PKG")

if [ x"$(echo $CHKSUM | cut -d" " -f1)" != x"$(echo $REMOTE_CHKSUM | cut -d" " -f1)" ]; then
    echo "$0: checksum mismatch - try again or check your connection"
    exit 3
fi
ssh $PACKAGES_HOST "sed -i /$VERSION-$REVISION/d $PACKAGES_PATH/sha256sum " \
                   "&& echo $REMOTE_CHKSUM >> $PACKAGES_PATH/sha256sum"
echo Package $PKG is now public at:
echo https://$PACKAGES_HOST/$PACKAGES_PROJECT/$PKG
