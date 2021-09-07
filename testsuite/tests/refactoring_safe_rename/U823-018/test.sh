# Rename the generic package declaration
safe_rename -P default.gpr -S gen.ads -L 3 -R 10 -N Genn --algorithm=analyse_ast
# Rename a declaration of a generic package declaration
safe_rename -P default.gpr -S gen.ads -L 4 -R 4 -N Constt --algorithm=analyse_ast
