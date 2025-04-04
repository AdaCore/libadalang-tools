#!/usr/bin/env bash

gnattest -P default.gpr
gprbuild -q -P gnattest/harness/test_driver.gpr
./gnattest/harness/test_runner
