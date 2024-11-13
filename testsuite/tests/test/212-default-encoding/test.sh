#!/bin/bash

# Simply generate a harness, and run it, we should have two tests.

gnattest -P prj.gpr
gprbuild -P obj/gnattest/harness/test_driver.gpr
./obj/gnattest/harness/test_runner
