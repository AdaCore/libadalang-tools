#!/bin/bash

echo "generate test driver"
gnattest -q -P contracts.gpr
echo "generate test driver with instrumentation"
gnattest -q -P contracts.gpr --dump-test-inputs
echo "build instrumented test driver"
gprbuild -q -P gnattest/harness/test_driver.gpr --src-subdirs=gnattest-instr --implicit-with=gnattest/harness/tgen_support/tgen_support.gpr

gnattest/harness/test_runner
if ls ./sqrt-*-2 1> /dev/null 2>&1; then
    echo "2nd test input present"
fi
