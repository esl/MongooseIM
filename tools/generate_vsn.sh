#!/bin/bash

# generate_vsn.sh [type]
# type = describe | tag | commits
#
# Certain 'type' values will produce following results:
# - With "describe" output being "3.0.0-233-g984dd6df7"...
# - "tag" will return "3.0.0"
# - "commits" will return "233"
# "describe" is the default value
#
# With git information missing, contents of VERSION file
# will be returned for "describe" and "tag"; "0" will be returned
# for "commits".

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
VSN_TYPE=${1:-"describe"}

echo_git_vsn() {
    if [ "$VSN_TYPE" == "tag" ]; then
        echo $LAST_TAG
    elif [ "$VSN_TYPE" == "commits" ]; then
        echo `git rev-list --count $LAST_TAG..HEAD 2>/dev/null`
    else
        echo `git describe --always --tags 2>/dev/null`
    fi
}

echo_file_vsn() {
	FILE_VSN=`cat ${DIR}/../VERSION`
    
    if [ "$VSN_TYPE" == "tag" ]; then
        echo $FILE_VSN
    elif [ "$VSN_TYPE" == "commits" ]; then
        echo "0"
    else
        echo $FILE_VSN
    fi
}

LAST_TAG=`git describe --tags --abbrev=0 2>/dev/null`

if [ $? -eq 0 ]; then
    echo_git_vsn
else
    echo_file_vsn
fi

