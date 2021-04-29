#!/bin/sh
safe_rename  -P default.gpr -S ./src/main.adb -L 5 -R 9 -N Y --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/main.adb -L 10 -R 12 -N A --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/main.adb -L 10 -R 12 -N Y --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/main.adb -L 12 -R 7 -N Var_A --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/main.adb -L 12 -R 7 -N Y --algorithm analyse_ast
