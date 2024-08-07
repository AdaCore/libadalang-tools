#!/bin/bash

echo "generate test driver"
gnattest -q -P complex.gpr
echo "generate test driver with instrumentation"
gnattest -q -P complex.gpr --dump-test-inputs
echo "build instrumented test driver"
gprbuild -q -P obj/gnattest/harness/test_driver.gpr --src-subdirs=gnattest-instr --implicit-with=obj/gnattest/harness/tgen_support/tgen_support.gpr

# By default, test inputs are dumped in the "tgen_test_inputs" dir
obj/gnattest/harness/test_runner --routines=matrix.ads:8
if ls ./tgen_test_inputs/add_matrix-*-1 1> /dev/null 2>&1; then
    echo "input for Add_Matrix dumped"
fi

obj/gnattest/harness/test_runner --routines=matrix.ads:10
if ls ./multiply_matrix-*-1 1> /dev/null 2>&1; then
    echo "Multiply_Matrix is not called, so no test input dump"
fi

# Test a couple of instances when the output dir is specified
obj/gnattest/harness/test_runner --routines=number-operations.ads:5 -o bar
if ls ./bar/multiply-*-3 1> /dev/null 2>&1; then
    echo "3 test inputs dumped for Multiply"
fi

echo "mix instrumentation with generated vectors"
gnattest -q -P complex.gpr --dump-test-inputs --gen-test-vectors
echo "build mixed test driver"
gprbuild -q -P obj/gnattest/harness/test_driver.gpr \
  --src-subdirs=gnattest-instr \
  --implicit-with=obj/gnattest/harness/tgen_support/tgen_support.gpr

obj/gnattest/harness/test_runner --routines=matrix.ads:8 -o foo/bar/
if ls ./foo/bar/add_matrix-*-t1 1> /dev/null 2>&1; then
    echo "input from auto-generated test for Add_Matrix dumped"
fi
