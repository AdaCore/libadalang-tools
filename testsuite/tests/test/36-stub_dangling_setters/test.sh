#! /bin/bash

# Generate a stubbing harness with the old sources, containing a custom type
gnattest -P prj.gpr -q -XSRC_DIR=old --stub

# Re-generate the harness but with the custom type dissapearing. Any remaining
# references to it will cause a compilation failure
gnattest -P prj.gpr -XSRC_DIR=new --stub

# Build the harness to check that the unused setters are not causing any
# problems.
gprbuild -P obj/gnattest_stub/harness/test_drivers.gpr -q -XSRC_DIR=new
