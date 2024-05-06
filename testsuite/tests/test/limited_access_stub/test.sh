#!/bin/sh

gnattest -P prj.gpr --stub -q
gprbuild -P obj/gnattest_stub/harness/test_drivers.gpr -q
