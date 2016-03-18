#!/usr/bin/env bash

## Propagate failures
set -e

TARGET=/tmp/mim-sandbox-system

rm -rf $TARGET || :
rm configure.out rel/configure.vars.config rel/vars.config || :
./tools/configure with-all user=erszcz prefix=$TARGET \
    system=yes && cat configure.out rel/configure.vars.config
rm -rf rel/mongooseim; RUNNER_GROUP=staff make install
cd $TARGET
git init
git commit --allow-empty -m "1st empty"
git add .
git commit -m "1st non-empty"
$TARGET/usr/bin/mongooseimctl start
$TARGET/usr/bin/mongooseimctl started && echo node: started
$TARGET/usr/bin/mongooseimctl status
$TARGET/usr/bin/mongooseimctl stop
$TARGET/usr/bin/mongooseimctl stopped && echo node: stopped
$TARGET/usr/bin/mongooseimctl status || echo status failed: this might be ok
retries=10
while ps aux | grep 'bea[m]' && [ $retries -gt 0 ]; do
    retries=$(expr $retries - 1)
    sleep 0.5s
done
ps aux | grep 'bea[m]' && { echo "node still running, but should not"; exit 1; }
git add .
git commit -m "2nd non-empty"
echo changed files: && git --no-pager show --format= --name-only
echo -n "tree: " && tree $TARGET -L 3
echo $0: all ok
