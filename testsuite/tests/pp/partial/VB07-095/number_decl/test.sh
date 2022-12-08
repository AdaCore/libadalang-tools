
echo "-----------------------------------------------------------------"
echo "       Number declarations testing"
partial_gnatpp -P default.gpr -S number_declaration.ads -SL 23 -EL 24
partial_gnatpp -P default.gpr -S number_declaration.ads -SL 26 -EL 26 -EC 36
partial_gnatpp -P default.gpr -S number_declaration.ads -SL 27 -EL 27 -EC 38
partial_gnatpp -P default.gpr -S number_declaration.ads -SL 28 -EL 28 -EC 38
partial_gnatpp -P default.gpr -S number_declaration.ads -SL 29 -EL 29 -EC 35
