#!/usr/bin/env bash

set -e

LALTOOLS_ROOT=$(dirname $(which gnattest))/..
TEMPLATES_PATH=$LALTOOLS_ROOT/share/tgen/templates
TGEN_LIGHT_OUTPUT=obj/tgen_light

# Generate test cases and dump inputs
gnattest -q -P user_project.gpr --gen-test-vectors --dump-test-inputs
make -C ./obj/gnattest/harness test_driver-build-inst &> /dev/null
./obj/gnattest/harness/test_runner

# Generate support files, but without JSON
mkdir -p $TGEN_LIGHT_OUTPUT/light_marshalling_lib
TGEN_NO_JSON_MARSHALLING=1 light_marshalling_lib user_project.gpr $TEMPLATES_PATH src/*.ads

# Copy the TGen light runtime
cp -r $LALTOOLS_ROOT/src/tgen/tgen_rts obj/light_marshalling_lib

# Build an executable that uses the light runtime. Here, we're building
# the tgen light runtime alongside the executable to keep the test
# simple.  In a real world scenario it would be better to build the
# light runtime, install it somewhere and make it available in the
# current environment.
gprbuild -q -P ./marshalling_support.gpr -j0
./obj/tgen_marshalling
