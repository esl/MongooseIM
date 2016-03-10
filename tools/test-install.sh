#!/usr/bin/env sh

set -e

TARGET=/tmp/mim-sandbox-system

rm -rf $TARGET
rm configure.out rel/configure.vars.config rel/vars.config
./tools/configure with-all user=erszcz prefix=$TARGET \
    system=yes && cat configure.out rel/configure.vars.config
rm -rf rel/mongooseim; RUNNER_GROUP=staff make install
cd $TARGET
git init
git commit --allow-empty -m "first empty"
git add .
git commit -m "first non-empty"
$TARGET/usr/bin/mongooseimctl start
$TARGET/usr/bin/mongooseimctl started
$TARGET/usr/bin/mongooseimctl status
$TARGET/usr/bin/mongooseimctl stop
$TARGET/usr/bin/mongooseimctl stopped
$TARGET/usr/bin/mongooseimctl status || echo node down: ok, this is expected
ps aux | grep 'bea[m]'
running=$?
if [ $running -eq 0 ];  then
	echo node still running, but should not
	exit 1
fi
git status
tree $TARGET -L 3
echo $0: all ok
