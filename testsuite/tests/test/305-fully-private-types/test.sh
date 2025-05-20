gnattest -q -P user_project.gpr --gen-test-vectors
gprbuild -q -P ./obj/gnattest/harness/test_driver.gpr
./obj/gnattest/harness/test_runner
