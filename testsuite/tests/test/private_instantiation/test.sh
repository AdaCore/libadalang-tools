#!/bin/bash

gnattest -q -P generics.gpr --stub importing.ads
gprbuild -q -P gnattest_stub/harness/test_drivers.gpr
