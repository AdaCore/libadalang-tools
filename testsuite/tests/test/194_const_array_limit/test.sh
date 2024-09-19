#!/bin/bash

gnattest -P prj.gpr --gen-test-vectors
gprbuild -P obj/gnattest/harness/test_driver.gpr -q
./obj/gnattest/harness/test_runner
