#!/bin/sh

gnattest -q -dd -Plib1 --stub
gprbuild -q -P obj/gnattest_stub/harness/test_drivers.gpr

gnattest -q -dd -Plib4 --stub
gprbuild -q -P obj-lib4/gnattest_stub/harness/test_drivers.gpr
