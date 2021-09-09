# Rename the parameter on an interface
safe_rename -P default.gpr -S foo.ads -L 3 -R 53 -N PP --algorithm=analyse_ast
# Rename the parameter on the spec of an interface implementation
safe_rename -P default.gpr -S foo.ads -L 5 -R 64 -N PP --algorithm=analyse_ast
# Rename the parameter on the body of an interface implementation
safe_rename -P default.gpr -S foo.adb -L 2 -R 64 -N PP --algorithm=analyse_ast
