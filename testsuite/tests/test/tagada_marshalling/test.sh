#!/bin/bash

LALTOOLS_ROOT=$(dirname $(which gnattest))/..
TEMPLATES_PATH=$LALTOOLS_ROOT/share/tgen/templates
mkdir -p test/obj obj
tgen_marshalling -P test/test.gpr --templates-dir=$TEMPLATES_PATH -o test/tgen_support test/my_file.ads test/show_date.ads
gprbuild -q -P test_gen.gpr
./obj/example_gen
# ./obj/example_introspection  Disabled until fixed (also modify test_gen.gpr)
