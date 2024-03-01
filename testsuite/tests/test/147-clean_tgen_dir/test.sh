# First, generate tests only for Int_Op
gnattest -P prj.gpr --gen-test-vectors -q int_op.ads

# Then instrument the whole harness to dump test cases.
# After this, the helpers for Str_Op will need marshallers for Standard.String.
gnattest -P prj.gpr --dump-test-inputs -q

# The re-generate a harness from the existing tests.
# As there are only tests for Int_Op, there should be no marshallers generated
# for Standard.String.
gnattest -P prj.gpr -q

# Then build the test harness. If there are lignering sources from step 2,
# compilation will fail as TGen.TGen_Support will not have the marshallers
# needed in Str_Op.TGen_Support.
gprbuild -P obj/gnattest/harness/test_driver.gpr -q
