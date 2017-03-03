#!/usr/bin/env bash

#set -x

# /builds is assumed to be a shared volume

BUILDS=${BUILDS:-/builds}
LOGFILE=${LOGFILE:-$BUILDS/build.log}
TIMESTAMP=$(date +%F_%H%M%S)

log () {
    echo \[$(date '+%F %H:%M:%S')\] $@
}

build () {
    local name=${1:-MongooseIM}
    local repo=${2:-https://github.com/esl/MongooseIM}
    local commit=${3:-master}
    log do_build: $name $commit $repo | tee -a $LOGFILE
    local workdir=/tmp/mongooseim
    local version_file=_build/prod/rel/mongooseim/version
    [ -d $workdir ] && rm -rf $workdir
    git clone $repo $workdir && \
        cd $workdir && \
        git checkout $commit && \
        tools/configure full && \
        make rel && \
        echo "${name}-${commit}-${repo}" > ${version_file} && \
        git describe --always >> ${version_file}
    local build_success=$?
    local timestamp=$(date +%F_%H%M%S)
    local tarball="mongooseim-${name}-${commit}-${timestamp}.tar.gz"
    if [ $build_success = 0 ]; then
        cd _build/prod/rel && \
            tar cfzh ${BUILDS}/${tarball} mongooseim && \
            log "${BUILDS}/$tarball is ready" && \
            exit 0
    else
        log "build failed"
        exit 1
    fi
    log "tarball generation failed"
    exit 2
}

build $@
