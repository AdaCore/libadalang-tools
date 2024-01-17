#!/bin/bash

LALTOOLS_ROOT=$(dirname $(which gnattest))/..
TEMPLATES_PATH=$LALTOOLS_ROOT/share/tgen/templates
mkdir -p test/obj/gnattest/tests/JSON_Tests obj
tgen_marshalling -P test/test.gpr --templates-dir=$TEMPLATES_PATH -o test/tgen_support test/pkg.ads
gnattest -P test/test.gpr --gen-test-vectors
gprbuild -q -P test/obj/gnattest/harness/test_driver.gpr
./test/obj/gnattest/harness/test_runner
