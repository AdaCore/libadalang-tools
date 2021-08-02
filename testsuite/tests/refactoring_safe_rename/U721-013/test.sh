# Hides a generic subprogram instantiation declared in a used package
safe_rename -P default.gpr -S baz.ads -L 5 -R 14 -N Integer_Swap --algorithm=analyse_ast
# Hides a rename of a generic subprogram instantiation declared in a used package
safe_rename -P default.gpr -S baz.ads -L 5 -R 14 -N I_Swap --algorithm=analyse_ast
# Valid rename - confirms that the rename works well when there are no renaming problems
safe_rename -P default.gpr -S baz.ads -L 5 -R 14 -N Swap_I --algorithm=analyse_ast
# Hides a generic subprogram instantiation declared in a used package
safe_rename -P default.gpr -S main.adb -L 7 -R 14 -N Integer_Swap --algorithm=analyse_ast
# Hides a rename of a generic subprogram instantiation declared in a used package
safe_rename -P default.gpr -S main.adb -L 7 -R 14 -N I_Swap --algorithm=analyse_ast
# Gets hidden by a generic subprogram instantiation declared in a visiable nested package
safe_rename -P default.gpr -S bar.ads -L 5 -R 14 -N Integer_Copy_To --algorithm=analyse_ast
# Collides with a generic subprogram instantiation of a different generic subprogram that has
# the same signature
safe_rename -P default.gpr -S main.adb -L 10 -R 14 -N Inst_2 --algorithm=analyse_ast
# Valid rename - confirms that renaming a generic subprogram instantiation to the same
# name as other generic subprogram instantiation of a different generic subprogram is
# allowed as long as both generic subprograms have different signatures.
safe_rename -P default.gpr -S main.adb -L 10 -R 14 -N Inst_3 --algorithm=analyse_ast
