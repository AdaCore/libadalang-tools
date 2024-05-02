#!/bin/bash

LALTOOLS_ROOT=$(dirname $(which gnattest))/..
TEMPLATES_PATH=$LALTOOLS_ROOT/share/tgen/templates
mkdir -p test/obj/gnattest/tests/JSON_Tests obj
tgen_marshalling -P test.gpr --templates-dir=$TEMPLATES_PATH -o obj/tgen_support system_under_test.ads
gprbuild -q -P obj/tgen_support/tgen_support.gpr
