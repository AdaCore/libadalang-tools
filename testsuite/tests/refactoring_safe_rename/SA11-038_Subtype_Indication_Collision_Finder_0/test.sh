#!/bin/sh
safe_rename  -P default.gpr -S ./src/a.ads -L 2 -R 9 -N My_Bar --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/a.ads -L 2 -R 9 -N My_Other_Bar --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/a.ads -L 2 -R 9 -N Yet_Another_Bar --algorithm analyse_ast
