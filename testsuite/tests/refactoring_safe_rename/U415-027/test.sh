# Foo (package with spec and body)
# Foo.Bar (package with spec only)
# Foo.Bar.Baz (generic package)
# Qux (generic procedure)
# Corge (procedure with spec and body)

# Foo
safe_rename -P default.gpr -S foo.ads -L 1 -R 9 -N Garply --algorithm=analyse_ast

# Bar
safe_rename -P default.gpr -S foo-bar.ads -L 3 -R 9 -N Garply --algorithm=analyse_ast

# Baz
safe_rename -P default.gpr -S main.adb -L 3 -R 14 -N Garply --algorithm=analyse_ast

# Qux
safe_rename -P default.gpr -S qux.adb -L 1 -R 12 -N Garply --algorithm=analyse_ast

# Corge
safe_rename -P default.gpr -S corge.ads -L 1 -R 11 -N Garply --algorithm=analyse_ast
