#!/bin/bash

cd test/
gnattest -Psimple simple.ads --gen-test-subprograms=simple.ads:5
cd obj/gnattest/harness
make > /dev/null
./test_runner --routines=simple.ads:4
