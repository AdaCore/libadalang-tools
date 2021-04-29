#!/bin/sh
safe_rename  -P default.gpr -S ./src/a.ads -L 22 -R 14 -N Bar_Procedure_1 --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/a.ads -L 22 -R 14 -N Bar_Procedure_2 --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/a.ads -L 22 -R 14 -N Bar_Function_2 --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/a.ads -L 26 -R 13 -N Bar_Procedure_2 --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/a.ads -L 26 -R 13 -N Bar_Function_1 --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/a.ads -L 26 -R 13 -N Bar_Function_2 --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/a.ads -L 26 -R 13 -N Bar_Function_3 --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/a.ads -L 26 -R 13 -N Bar_Function_4 --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/a.ads -L 30 -R 13 -N Bar_Function_1 --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/a.ads -L 30 -R 13 -N Bar_Function_3 --algorithm analyse_ast
