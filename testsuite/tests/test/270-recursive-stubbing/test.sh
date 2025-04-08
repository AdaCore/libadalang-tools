#!/bin/bash

_exit() {
    printf $*
    exit 1
}

cd test/

# First instrumentation without --recursive stub, there should only be `pkg_b`
# unit files in the stub folder.
gnattest -P proj.gpr --stub pkg_a.ads

find obj/gnattest_stub/stubs/Proj/ -name '*pkg_b*' | grep -q .
[ $? = 0 ] || _exit "Missing expected stub files related to \`pkg_b\`."

find obj/gnattest_stub/stubs/Proj/ -name '*pkg_c*' | grep -q .
[ $? = 0 ] && _exit "Found unexpected stub files related to \`pkg_c\`."

# Compile the project and see if running it causes a divide by zero error,
# which is the expected behavior in case Pkg_B is elaborated using the original
# Pkg_C unit.
OUT="$(gprbuild -P obj/gnattest_stub/harness/Pkg_A.Test_Data.Tests/test_driver.gpr 2>&1 || true)"
[ $? = 0 ] || _exit "Unable to compile the project:\n%s" "$OUT"

OUT=$(./obj/gnattest_stub/harness/Pkg_A.Test_Data.Tests/pkg_a-test_data-tests-suite-test_runner 2>&1)
echo "$OUT" | grep -q -E 'CONSTRAINT_ERROR :.* divide by zero'
[ $? = 0 ] || _exit "Expected divide by zero error, Got\n$OUT"

# Second instrumentation with --recursive-stub, there should be `pkg_[bc]`
# unit files in the stub folder.
gnattest -P proj.gpr --stub --recursive-stub pkg_a.ads

find obj/gnattest_stub/stubs/Proj/ -name '*pkg_c*' | grep -q .
[ $? = 0 ] || _exit "Missing expected stub files related to \`pkg_c\`."

# Compile the project. Now, if the recursive stubbing works correctly, the
# program should fail for another reason than divide by zero.
OUT="$(gprbuild -P obj/gnattest_stub/harness/Pkg_A.Test_Data.Tests/test_driver.gpr 2>&1 || true)"
[ $? = 0 ] || _exit "Unable to compile the project:\n%s" "$OUT"

OUT=$(./obj/gnattest_stub/harness/Pkg_A.Test_Data.Tests/pkg_a-test_data-tests-suite-test_runner 2>&1)
echo "$OUT" | grep -q -E 'CONSTRAINT_ERROR :.* divide by zero'
[ $? = 0 ] && _exit "Unexpected divide by zero error, Got\n$OUT"

printf "success"
