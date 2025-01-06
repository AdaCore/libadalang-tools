#!/usr/bin/env bash

gnattest -P build.gpr \
    --gen-test-vectors \
    --gen-test-subprograms=pkg.ads:2,pkg.ads:3
gprbuild -q -P ./gnattest/harness/test_driver.gpr
./gnattest/harness/test_runner
