#!/bin/sh
safe_rename  -P default.gpr -S ./src/a.ads -L 3 -R 19 -N Bar --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/a.ads -L 4 -R 23 -N My_Other_Bar --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/a.ads -L 4 -R 23 -N Yet_Another_Bar --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/a.ads -L 4 -R 51 -N My_Other_Bar --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/a.ads -L 4 -R 31 -N My_Bar_Foo --algorithm analyse_ast
