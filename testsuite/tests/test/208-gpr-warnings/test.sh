#!/bin/bash

gnattest -P prj.gpr && gprbuild -q -P obj/gnattest/harness/test_driver.gpr
