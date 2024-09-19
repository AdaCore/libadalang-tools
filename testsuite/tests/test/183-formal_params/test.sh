#! /bin/bash

# We expect only diagnostics stating that there are no
# subprograms to test, not that some type is unsupported.
gnattest -P prj.gpr --gen-test-vectors
