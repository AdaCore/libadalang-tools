#!/usr/bin/env bash

GT_OUT=gt.out

gnattest -q -P build.gpr --gen-test-vectors &> $GT_OUT
gprbuild -q -P gnattest/harness/test_driver.gpr
./gnattest/harness/test_runner

grep 'warning: (gnattest) top_level_child_generic_child.ads:1:1: no instance of Top_Level_Child_Generic_Child' $GT_OUT

# grep returns 1 when no lines are selected which is expected here.
test $? -eq 1
