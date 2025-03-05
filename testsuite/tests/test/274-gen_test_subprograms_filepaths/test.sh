#!/usr/bin/env bash

BAZ_ABSOLUTE_PATH=$(realpath ./src/nested/baz.ads)

gnattest \
    -q \
    -P prj.gpr \
    --gen-test-vectors \
    --gen-test-subprograms=./src/foo.ads:2,./src/nested/bar.ads:2,$BAZ_ABSOLUTE_PATH:2
gprbuild -q -P ./gnattest/harness/test_driver.gpr
./gnattest/harness/test_runner
