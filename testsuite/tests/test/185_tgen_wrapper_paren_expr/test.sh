#!/bin/bash

LALTOOLS_ROOT=$(dirname $(which gnattest))/..
TEMPLATES_PATH=$LALTOOLS_ROOT/share/tgen/templates
mkdir -p test/obj obj
tgen_marshalling -P test/test.gpr --templates-dir=$TEMPLATES_PATH -o test/tgen_support test/simple.ads
gprbuild -q -P test_wrapper.gpr -cargs -gnata
