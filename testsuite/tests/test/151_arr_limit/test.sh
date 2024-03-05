#!/bin/bash

LALTOOLS_ROOT=$(dirname $(which gnattest))/..
TEMPLATES_PATH=$LALTOOLS_ROOT/share/tgen/templates
mkdir -p test/obj obj
# Set a very low limit in the array size, writing arrays over this size is not
# impacted, but we should not be able to read them back.
export TGEN_ARRAY_LIMIT=3
tgen_marshalling -P test/test.gpr --templates-dir=$TEMPLATES_PATH -o test/tgen_support test/my_file.ads
gprbuild -q -P test_gen.gpr
./obj/example_gen
