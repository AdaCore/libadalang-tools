#! /bin/bash

# First, run gnattest in stub mode, with tests placed in source subdirectories,
# and stubs in the object dir of the stubbed project.
gnattest -P prj.gpr --stub --subdirs=test --stubs-dir=stub -q

# The build both test driver aggregate projects, to ensure the generated
# harnesses are valid.
gprbuild -q obj1/gnattest_stub/harness/test_drivers.gpr
gprbuild -q obj2/gnattest_stub/harness/test_drivers.gpr
