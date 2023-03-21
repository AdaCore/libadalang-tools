#!/bin/sh
safe_rename -P test.gpr -S ./test.ads -L 2 -R 4 -N b --algorithm analyse_ast
