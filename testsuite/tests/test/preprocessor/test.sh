#!/usr/bin/env bash

gnattest -q -P user_project.gpr --gen-test-vectors
# Build harness
gprbuild -q -P obj/gnattest/harness/test_driver.gpr
