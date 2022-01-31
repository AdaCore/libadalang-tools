#!/bin/sh
safe_rename  -P default.gpr -S ./src/b-z.adb -L 3 -R 16 -N C --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/b-z.adb -L 3 -R 16 -N C_Private --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/b-z.adb -L 3 -R 16 -N K --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/b-z.adb -L 3 -R 16 -N R --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/b-z.adb -L 3 -R 16 -N U --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/b.ads -L 13 -R 12 -N C --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/b.ads -L 13 -R 12 -N K --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/b.ads -L 13 -R 12 -N C_Private --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/b.ads -L 13 -R 12 -N L --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/b.ads -L 13 -R 12 -N Z --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/b.ads -L 13 -R 12 -N U --algorithm analyse_ast
safe_rename  -P default.gpr -S ./src/b.ads -L 13 -R 12 -N R --algorithm analyse_ast
