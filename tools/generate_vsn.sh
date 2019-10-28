#!/bin/sh

cd `dirname "$0"`/../

GIT_VSN=`git describe --always --tags 2>/dev/null`

if [ $? -eq 0 ]; then
	echo $GIT_VSN
else
	cat VERSION
fi
