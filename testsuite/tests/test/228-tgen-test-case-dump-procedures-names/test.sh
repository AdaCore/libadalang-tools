#!/usr/bin/env bash

LALTOOLS_ROOT=$(dirname $(which gnattest))/..
TEMPLATES_PATH=$LALTOOLS_ROOT/share/tgen/templates
export GPR_PROJECT_PATH="$LALTOOLS_ROOT/src:$GPR_PROJECT_PATH"

mkdir -p obj/tgen
gprbuild -q -P build.gpr
./main user.gpr $TEMPLATES_PATH src/*.ads
