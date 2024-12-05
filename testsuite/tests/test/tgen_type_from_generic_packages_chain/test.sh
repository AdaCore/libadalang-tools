#!/usr/bin/env bash

gnattest -q -P build.gpr --gen-test-vectors
cd gnattest/harness
gprbuild -q -P test_driver.gpr
./test_runner
