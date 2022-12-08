
echo "-----------------------------------------------------------------"
echo "       Subtype declarations testing"
partial_gnatpp -P default.gpr -S subtype_declarations.ads -SL 25 -EL 26
partial_gnatpp -P default.gpr -S subtype_declarations.ads -SL 29 -EL 30
partial_gnatpp -P default.gpr -S subtype_declarations.ads -SL 33 -EL 34
partial_gnatpp -P default.gpr -S subtype_declarations.ads -SL 37 -EL 38
partial_gnatpp -P default.gpr -S subtype_declarations.ads -SL 60 -EL 61
partial_gnatpp -P default.gpr -S subtype_declarations.ads -SL 70 -EL 71

