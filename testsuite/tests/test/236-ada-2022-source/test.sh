#!/usr/bin/env bash

gnattest -q -gnat2022 -P build.gpr --gen-test-vectors
gprbuild -q -P ./gnattest/harness/test_driver.gpr
./gnattest/harness/test_runner