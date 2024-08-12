#!/bin/bash

echo "generate test driver"
gnattest -q -P simple.gpr
echo "generate test driver with instrumentation"
gnattest -q -P simple.gpr --dump-test-inputs --passed-tests=hide
echo "build instrumented test driver"
gprbuild -q -P gnattest/harness/test_driver.gpr --src-subdirs=gnattest-instr --implicit-with=gnattest/harness/tgen_support/tgen_support.gpr

gnattest/harness/test_runner --routines=p.ads:3
if ls ./tgen_test_inputs/fibonacci-*-1 1> /dev/null 2>&1; then
    echo "input for expr func as body dumped"
fi

gnattest/harness/test_runner --routines=p.ads:6
if ls ./tgen_test_inputs/fibonacci_rename-*-1 1> /dev/null 2>&1; then
    echo "input for renaming as body dumped"
fi


gnattest/harness/test_runner --routines=p.ads:9
if ls ./tgen_test_inputs/stub_body-*-1 1> /dev/null 2>&1; then
    echo "input for null procedure as body dumped"
fi

