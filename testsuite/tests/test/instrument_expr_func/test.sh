#!/bin/bash

echo "generate test driver"
gnattest -q -P simple.gpr
echo "generate test driver with instrumentation"
gnattest -q -P simple.gpr --dump-test-inputs --passed-tests=hide
echo "build instrumented test driver"
gprbuild -q -P gnattest/harness/test_driver.gpr --src-subdirs=gnattest-instr --implicit-with=gnattest/harness/tgen_support/tgen_support.gpr

gnattest/harness/test_runner --routines=p.ads:3
if ls ./tgen_test_inputs/expr-*-1 1> /dev/null 2>&1; then
    echo "input for expr func dumped"
fi

gnattest/harness/test_runner --routines=p.ads:6
if ls ./tgen_test_inputs/expr_private_body-*-1 1> /dev/null 2>&1; then
    echo "input for func with private expr func as body dumped"
fi


gnattest/harness/test_runner --routines=p.ads:23
if ls ./tgen_test_inputs/expr_nested_no_package_body_2-*-1 1> /dev/null 2>&1; then
    echo "input for nested expr func dumped"
fi

gnattest/harness/test_runner --routines=q.ads:2
if ls ./tgen_test_inputs/inc-*-1 1> /dev/null 2>&1; then
    echo "input for expr func from bodyless spec dumped"
fi

gnattest/harness/test_runner --routines=q.ads:10
if ls ./tgen_test_inputs/f5-*-1 1> /dev/null 2>&1; then
    echo "input for nested expr func from bodyless spec dumped"
fi