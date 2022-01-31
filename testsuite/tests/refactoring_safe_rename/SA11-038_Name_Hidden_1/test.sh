#!/bin/sh
safe_rename  -P default.gpr -S ./src/foo.ads -L 6 -R 4 -N Flob --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/foo.ads -L 13 -R 7 -N Flob --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/foo.ads -L 16 -R 10 -N Flob --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/foo.ads -L 20 -R 10 -N Flob --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/foo.ads -L 23 -R 7 -N Flob --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/foo.ads -L 26 -R 4 -N Flob --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/foo.ads -L 28 -R 4 -N Flob --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/foo.adb -L 11 -R 4 -N Flob --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/foo.adb -L 22 -R 7 -N Flob --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/foo.adb -L 26 -R 10 -N Flob --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/foo.adb -L 29 -R 13 -N Flob --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/foo.adb -L 28 -R 27 -N Flob --algorithm analyse_ast
