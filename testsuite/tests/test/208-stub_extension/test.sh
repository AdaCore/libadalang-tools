#! /bin/sh

gnattest -q -P failing.gpr --stub
gprbuild -q -P gnattest_stub/harness/test_drivers.gpr
