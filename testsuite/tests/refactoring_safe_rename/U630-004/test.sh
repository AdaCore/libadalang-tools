# Rename the first identifier of a dotted name
safe_rename -P default.gpr -S lsp-ada_completion_sets.ads -L 1 -R  9 -N Ada_LSP --algorithm=analyse_ast
# Rename the last identifier of a dotted name
safe_rename -P default.gpr -S lsp-ada_completion_sets.ads -L 1 -R 13 -N Completion_Sets --algorithm=analyse_ast
