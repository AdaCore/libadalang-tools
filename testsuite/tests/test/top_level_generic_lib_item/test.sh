gnattest -q -P build.gpr --gen-test-vectors
test $? -eq 1
