# Use clause of a package with multiple compilation units

# Hides a declaration in bar.ada
safe_rename -P default.gpr -S main.adb -L 3 -R 14 -N Qux --algorithm=analyse_ast
