#!/bin/bash

LALTOOLS_ROOT=$(dirname $(which gnattest))/..
TEMPLATES_PATH=$LALTOOLS_ROOT/share/tgen/templates
RTS_PATH=$LALTOOLS_ROOT/share/tgen/tgen_rts
export GPR_PROJECT_PATH=$RTS_PATH:$GPR_PROJECT_PATH
mkdir -p test/obj obj
tgen_marshalling -P test/test.gpr --templates-dir=$TEMPLATES_PATH -o test/tgen_support test/my_file.ads
gprbuild -q -P test_gen.gpr
./obj/example_gen
