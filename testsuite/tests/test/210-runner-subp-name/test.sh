#!/bin/sh

gnattest -P prj.gpr --include-subp-name --skeleton-default=pass --validate-type-extensions -q --gen-test-vectors
gprbuild -P obj/gnattest/harness/test_driver.gpr -q
./obj/gnattest/harness/test_runner
