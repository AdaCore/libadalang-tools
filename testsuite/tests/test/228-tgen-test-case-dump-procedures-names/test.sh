#!/usr/bin/env bash

LALTOOLS_ROOT=$(dirname $(which gnattest))/..
TEMPLATES_PATH=$LALTOOLS_ROOT/share/tgen/templates

tgen_dump_proc_name user.gpr $TEMPLATES_PATH src/*.ads
