#!/bin/sh

gnattest -q -dd -Plib1 --stub
gprbuild -q -P obj/gnattest_stub/harness/test_drivers.gpr
