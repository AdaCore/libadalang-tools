echo "# All tests failing #"

echo "-- Enabling --exit-status"

gnattest -P test.gpr --stub --exit-status=on --tests-dir ../tests -q
gprbuild -q -P obj/gnattest_stub/harness/test_drivers.gpr
(gnattest obj/gnattest_stub/harness/test_drivers.list --exit-status=on; echo "Exit status: $? (should be 1)")

echo "-- Disabling --exit-status"

gnattest -P test.gpr --stub --exit-status=off --tests-dir ../tests -q
gprbuild -q -P obj/gnattest_stub/harness/test_drivers.gpr
gnattest obj/gnattest_stub/harness/test_drivers.list
echo "Exit status: $? (should be 0)"
