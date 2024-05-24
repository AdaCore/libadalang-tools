#!/bin/sh

gnattest -P prj.gpr --gen-test-vectors -q
gprbuild -P obj/gnattest/harness/test_driver.gpr -q
./obj/gnattest/harness/test_runner
