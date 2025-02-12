#!/bin/bash

gnattest -q -P test1.gpr
gnattest -q -P test2.gpr --subdirs="my_tests"
