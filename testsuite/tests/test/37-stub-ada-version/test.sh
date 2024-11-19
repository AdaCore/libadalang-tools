#!/bin/bash

gnattest -P prj.gpr --stub -q --stubs-dir=./stub_default
gprbuild -P obj/gnattest_stub/harness/test_drivers.gpr -q

# Check we have Ada_2012 by default
grep "pragma Ada_2012;" ./obj/stub_default/Prj/dep-stub_data.ads

# Same thing, but with Ada_2005
gnattest -P prj.gpr --stub -q --stubs-dir=./stub_05 -gnat05
gprbuild -P obj/gnattest_stub/harness/test_drivers.gpr -q

grep "pragma Ada_2005;" ./obj/stub_05/Prj/dep-stub_data.ads
