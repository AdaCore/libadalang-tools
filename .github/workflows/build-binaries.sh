#!/bin/bash
set -x -e
DEBUG=$1  # Value is '' or 'debug'
RUNNER_OS=$2  #  ${{ runner.os }} is Linux, Windiws, maxOS
prefix=/tmp/ADALIB_DIR

if [ $RUNNER_OS = Windows ]; then
    prefix=/opt/ADALIB_DIR
    mount `cmd /c cd | cut -d\: -f1`:/opt /opt
fi

export GPR_PROJECT_PATH=$prefix/share/gpr
export CPATH=/usr/local/include:/mingw64/include
export LIBRARY_PATH=/usr/local/lib:/mingw64/lib
export DYLD_LIBRARY_PATH=/usr/local/lib
BRANCH=stable
mkdir -p $prefix
FILE=libadalang-$RUNNER_OS-$BRANCH${DEBUG:+-dbg}-static.tar.gz
aws s3 cp s3://adacore-gha-tray-eu-west-1/libadalang/$FILE . --sse=AES256
tar xzf $FILE -C $prefix
rm -f -v $FILE
gprinstall --uninstall gnatcoll || true
gprinstall --uninstall gpr || true

if [ "$DEBUG" = "debug" ]; then
    export BUILD_MODE=dev
else
    export BUILD_MODE=prod
fi

make LIBRARY_TYPE=static all
