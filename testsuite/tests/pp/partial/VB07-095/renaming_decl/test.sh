
echo "-----------------------------------------------------------------"
echo "       Renaming declarations testing"
partial_gnatpp -P default.gpr -S src/renaming_declaration.adb -SL 27 -EL 28
partial_gnatpp -P default.gpr -S src/renaming_declaration.adb -SL 29 -EL 29 -EC 58

echo ""
echo "-----------------------------------------------------------------"
echo "       Object renaming declarations testing"
partial_gnatpp -P default.gpr -S src/object_renaming_declaration.ads -SL 41 -EL 42
partial_gnatpp -P default.gpr -S src/object_renaming_declaration.ads -SL 42 -EL 43
partial_gnatpp -P default.gpr -S src/object_renaming_declaration.ads -SL 44 -EL 45

partial_gnatpp -P default.gpr -S src/object_renaming_declaration.ads -SL 46 -EL 47
partial_gnatpp -P default.gpr -S src/object_renaming_declaration.ads -SL 47 -EL 48

partial_gnatpp -P default.gpr -S src/object_renaming_declaration.ads -SL 49 -EL 50

# this is not yet supported
#partial_gnatpp -P default.gpr -S src/object_renaming_declaration.ads -SL 50 -EL 51

partial_gnatpp -P default.gpr -S src/object_renaming_declaration.ads -SL 52 -EL 53
partial_gnatpp -P default.gpr -S src/object_renaming_declaration.ads -SL 53 -EL 54
partial_gnatpp -P default.gpr -S src/object_renaming_declaration.ads -SL 54 -EL 55

partial_gnatpp -P default.gpr -S src/object_renaming_declaration.ads -SL 56 -EL 57
partial_gnatpp -P default.gpr -S src/object_renaming_declaration.ads -SL 57 -EL 58

echo ""
echo "-----------------------------------------------------------------"
echo "       Package renaming declarations testing"
partial_gnatpp -P default.gpr -S src/package_renaming_declaration.ads -SL 26 -EL 26 -EC 68

echo ""
echo "-----------------------------------------------------------------"
echo "       Subprogram renaming declarations testing"
partial_gnatpp -P default.gpr -S src/subprogram_renaming_declaration.ads -SL 31 -EL 32
partial_gnatpp -P default.gpr -S src/subprogram_renaming_declaration.ads -SL 32 -EL 33

echo ""
echo "-----------------------------------------------------------------"
echo "       Generic renaming declarations testing"
partial_gnatpp -P default.gpr -S src/generic_renaming_declaration.ads -SL 31 -EL 31 -EC 82

echo ""
echo "-----------------------------------------------------------------"
echo "       Exception renaming declarations testing"
partial_gnatpp -P default.gpr -S src/exception_renaming_declaration.ads -SL 30 -EL 31









