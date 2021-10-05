# Rename every identifier that is used in a top level decl except the main
# procedure

# Foo
safe_rename -P default.gpr -S main.adb -L 1 -R 6 -N Fox --algorithm=analyse_ast
# Baz
safe_rename -P default.gpr -S main.adb -L 2 -R 10 -N Fox --algorithm=analyse_ast
# Qux
safe_rename -P default.gpr -S main.adb -L 3 -R 10 -N Fox --algorithm=analyse_ast
# Garply
safe_rename -P default.gpr -S main.adb -L 4 -R 14 -N Fox --algorithm=analyse_ast
