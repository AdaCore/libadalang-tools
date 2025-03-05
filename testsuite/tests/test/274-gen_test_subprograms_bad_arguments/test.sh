#!/usr/bin/env bash

gnattest \
    -P user_project.gpr \
    --gen-test-vectors \
    --gen-test-subprograms=./src/foo.ads
gnattest \
    -P user_project.gpr \
    --gen-test-vectors \
    --gen-test-subprograms=./src/foo.ads:abc
