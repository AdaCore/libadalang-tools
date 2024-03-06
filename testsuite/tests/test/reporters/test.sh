#!/bin/bash

echo "Driver per unit:"
gnattest -P tagged_rec.gpr -q --harness-dir=h1 --reporter=xml --separate-drivers=unit
gprbuild -q -P h1/test_drivers.gpr
h1/P.T_Test_Data.T_Tests/p-t_test_data-t_tests-suite-test_runner
echo "Driver per test:"
gnattest -P tagged_rec.gpr -q --harness-dir=h2 --reporter=text --separate-drivers=test --validate-type-extensions
gprbuild -q -P h2/test_drivers.gpr
h2/Q.T2_Test_Data.T2_Tests/q-t2_test_data-t2_tests-driver_test_x2_0cbc93
echo "Driver per test (substitution check):"
h2/Q.T2_Test_Data.T2_Tests/q-t2_test_data-t2_tests-vte_driver_test_x2_0cbc93
echo "Driver per unit (stubbing):"
gnattest -P tagged_rec.gpr -q --harness-dir=h3 --reporter=xml --stub
gprbuild -q -P h3/test_drivers.gpr
h3/P.T_Test_Data.T_Tests/p-t_test_data-t_tests-suite-test_runner
echo "Driver per test (stubbing):"
gnattest -P tagged_rec.gpr -q --harness-dir=h4 --reporter=text --stub --separate-drivers=test
gprbuild -q -P h4/test_drivers.gpr
h4/P.T_Test_Data.T_Tests/p-t_test_data-t_tests-driver_test_x1_09503c
