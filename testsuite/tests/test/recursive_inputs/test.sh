#!/bin/bash

echo "generate test driver"
gnattest -q -P simple.gpr
echo "generate test driver with instrumentation"
gnattest -q -P simple.gpr --dump-test-inputs
echo "build instrumented test driver"
gprbuild -q -P obj/gnattest/harness/test_driver.gpr --src-subdirs=gnattest-instr --implicit-with=obj/gnattest/harness/tgen_support/tgen_support.gpr

obj/gnattest/harness/test_runner --routines=p.ads:3
if ls ./fibonacci-*-1 1> /dev/null 2>&1; then
    echo "1st input for Fibonacci dumped"
fi
if ls ./fibonacci-*-2 1> /dev/null 2>&1; then
    echo "Subsequent inputs for Fibonacci dumped (error)"
fi

obj/gnattest/harness/test_runner --routines=p.ads:6
if ls ./factorial-*-1 1> /dev/null 2>&1; then
    echo "1st input for Factorial dumped"
fi
if ls ./factorial-*-2 1> /dev/null 2>&1; then
    echo "2nd input for Factorial dumped (after exception in 1st call)"
fi
if ls ./factorial-*-3 1> /dev/null 2>&1; then
    echo "Subsequent inputs for Factorial dumped (error)"
fi
