# Rename an expression function parameter

# No conflicts
safe_rename -P default.gpr -S main.adb -L 6 -R 16 -N W --algorithm=analyse_ast

# Hides a declaration on the same scope as the expression function
safe_rename -P default.gpr -S main.adb -L 6 -R 16 -N A --algorithm=analyse_ast

# Conflict with another parameter
safe_rename -P default.gpr -S main.adb -L 6 -R 16 -N Y --algorithm=analyse_ast
safe_rename -P default.gpr -S main.adb -L 6 -R 16 -N Z --algorithm=analyse_ast

# No conflicts with declarations that are still not visible
safe_rename -P default.gpr -S main.adb -L 6 -R 16 -N B --algorithm=analyse_ast
