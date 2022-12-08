
echo "-----------------------------------------------------------------"
echo "       Generic instantiation testing"
partial_gnatpp -P default.gpr -S src/generic_instantiation.ads -SL 25 -EL 26
partial_gnatpp -P default.gpr -S src/generic_instantiation.ads -SL 26 -EL 27
partial_gnatpp -P default.gpr -S src/generic_instantiation.ads -SL 27 -EL 28
partial_gnatpp -P default.gpr -S src/generic_instantiation.ads -SL 29 -EL 30
echo ""
echo "-----------------------------------------------------------------"
echo "       Generic package declaration testing"
partial_gnatpp -P default.gpr -S src/generic_package_declaration.ads -SL 20 -EL 31 -EC 33
echo ""
echo "-----------------------------------------------------------------"
echo "       Generic subprogram declaration testing"
partial_gnatpp -P default.gpr -S src/generic_subprogram_declaration.ads -SL 22 -EL 25
partial_gnatpp -P default.gpr -S src/generic_subprogram_declaration.ads -SL 26 -EL 31
