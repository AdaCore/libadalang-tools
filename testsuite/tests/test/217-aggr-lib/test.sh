#!/bin/bash

gnattest -P root.gpr --stub --stubs-dir ../stubs --tests-dir ../tests -q
gprbuild -P obj/gnattest_stub/harness/test_drivers.gpr -q
gnattest obj/gnattest_stub/harness/test_drivers.list
