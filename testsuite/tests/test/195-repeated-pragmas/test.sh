#!/usr/bin/env bash

set -e

STUB_LOCATION='obj/gnattest_stub/stubs/Input/input-stub_data.ads'

gnattest --stub -P ops.gpr 2> /dev/null
cat $STUB_LOCATION
gnattest --stub -P ops.gpr 2> /dev/null
cat $STUB_LOCATION
