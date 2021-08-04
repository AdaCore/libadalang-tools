# Name collision with a sibbling enum literal
safe_rename -P default.gpr -S main.adb -L 2 -R 17 -N B --algorithm=analyse_ast
# Valid rename - no name collision with other declarations
safe_rename -P default.gpr -S main.adb -L 2 -R 17 -N D --algorithm=analyse_ast
