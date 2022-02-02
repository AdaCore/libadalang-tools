# Rename both from implementation
safe_rename -P default.gpr -S bar.ads -L 5 -R 13 -N Qux --algorithm=analyse_ast
# Rename one from implementation 1
safe_rename -P default.gpr -S bar.ads -L 8 -R 13 -N Qux --algorithm=analyse_ast
# Rename one from implementation 2
safe_rename -P default.gpr -S bar.ads -L 11 -R 13 -N Qux --algorithm=analyse_ast
# Rename one from interface 1
safe_rename -P default.gpr -S foos.ads -L 3 -R 13 -N Qux --algorithm=analyse_ast
# Rename one from interface 2
safe_rename -P default.gpr -S foos.ads -L 5 -R 13 -N Qux --algorithm=analyse_ast
