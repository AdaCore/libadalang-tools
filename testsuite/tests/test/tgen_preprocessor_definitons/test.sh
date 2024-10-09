#!/usr/bin/env bash

rm -rf obj
gnattest -q -P user_project.gpr --gen-test-vectors
