# Simply test that gnattest does not crash when generating test vectors and
# make sure that the generated tests are working
gnattest -P test.gpr --gen-test-vectors -q
gprbuild -q -P ./obj/gnattest/harness/test_driver.gpr
./obj/gnattest/harness/test_runner
