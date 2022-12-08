
echo "-----------------------------------------------------------------"
echo "       Subprogram declarations testing"
partial_gnatpp -P default.gpr -S src/subprogram_declaration.ads -SL 23 -EL 24
partial_gnatpp -P default.gpr -S src/subprogram_declaration.ads -SL 24 -EL 25
partial_gnatpp -P default.gpr -S src/subprogram_declaration.ads -SL 25 -EL 26
partial_gnatpp -P default.gpr -S src/subprogram_declaration.ads -SL 26 -EL 27

partial_gnatpp -P default.gpr -S src/subprogram_declaration.ads -SL 28 -EL 29

partial_gnatpp -P default.gpr -S src/subprogram_declaration.ads -SL 30 -EL 31
partial_gnatpp -P default.gpr -S src/subprogram_declaration.ads -SL 31 -EL 32
partial_gnatpp -P default.gpr -S src/subprogram_declaration.ads -SL 32 -EL 33
partial_gnatpp -P default.gpr -S src/subprogram_declaration.ads -SL 33 -EL 34

partial_gnatpp -P default.gpr -S src/subprogram_declaration.ads -SL 35 -EL 36

partial_gnatpp -P default.gpr -S src/subprogram_declaration.ads -SL 37 -EL 41

echo "-----------------------------------------------------------------"
echo "       Null procedure declaration testing"
partial_gnatpp -P default.gpr -S src/null_procedure_declaration.ads -SL 23 -EL 24

echo "-----------------------------------------------------------------"
echo "       Abstract subprogram declaration testing"
partial_gnatpp -P default.gpr -S src/abstract_subprogram_declaration.ads -SL 22 -EL 23
partial_gnatpp -P default.gpr -S src/abstract_subprogram_declaration.ads -SL 23 -EL 24

partial_gnatpp -P default.gpr -S src/abstract_subprogram_declaration.ads -SL 25 -EL 26
partial_gnatpp -P default.gpr -S src/abstract_subprogram_declaration.ads -SL 26 -EL 27
partial_gnatpp -P default.gpr -S src/abstract_subprogram_declaration.ads -SL 27 -EL 28
partial_gnatpp -P default.gpr -S src/abstract_subprogram_declaration.ads -SL 28 -EL 29
partial_gnatpp -P default.gpr -S src/abstract_subprogram_declaration.ads -SL 29 -EL 31

echo "-----------------------------------------------------------------"
echo "       Expression function declaration testing"
partial_gnatpp -P default.gpr -S src/expression_function_declaration.ads -SL 28 -EL 31
partial_gnatpp -P default.gpr -S src/expression_function_declaration.ads -SL 32 -EL 34







