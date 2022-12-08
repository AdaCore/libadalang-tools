
echo "-----------------------------------------------------------------"
echo "       Object declarations testing"
echo "    *  Multiple object declarations"
partial_gnatpp -P default.gpr -S src/object_declarations.ads -SL 56 -EL 57
partial_gnatpp -P default.gpr -S src/object_declarations.ads -SL 57 -EL 58
echo ""
echo "    *  Variables declarations"
partial_gnatpp -P default.gpr -S src/object_declarations.ads -SL 63 -EL 64
partial_gnatpp -P default.gpr -S src/object_declarations.ads -SL 64 -EL 65
partial_gnatpp -P default.gpr -S src/object_declarations.ads -SL 65 -EL 66
partial_gnatpp -P default.gpr -S src/object_declarations.ads -SL 69 -EL 70
partial_gnatpp -P default.gpr -S src/object_declarations.ads -SL 72 -EL 73
partial_gnatpp -P default.gpr -S src/object_declarations.ads -SL 74 -EL 75
partial_gnatpp -P default.gpr -S src/object_declarations.ads -SL 75 -EL 76
echo ""
echo "    *  Constants declarations"
partial_gnatpp -P default.gpr -S src/object_declarations.ads -SL 81 -EL 82
partial_gnatpp -P default.gpr -S src/object_declarations.ads -SL 82 -EL 83
partial_gnatpp -P default.gpr -S src/object_declarations.ads -SL 83 -EL 84
partial_gnatpp -P default.gpr -S src/object_declarations.ads -SL 84 -EL 85
partial_gnatpp -P default.gpr -S src/object_declarations.ads -SL 85 -EL 86

echo ""
echo "-----------------------------------------------------------------"
echo "       Single protected declarations testing"
partial_gnatpp -P default.gpr -S src/single_protected_decl.adb -SL 36 -EL 44
partial_gnatpp -P default.gpr -S src/single_protected_decl.adb -SL 45 -EL 58

partial_gnatpp -P default.gpr -S src/single_protected_decl.adb -SL 59 -EL 66
partial_gnatpp -P default.gpr -S src/single_protected_decl.adb -SL 67 -EL 78

echo ""
echo "-----------------------------------------------------------------"
echo "       Single task declarations testing"
partial_gnatpp -P default.gpr -S src/single_task_declaration.adb -SL 33 -EL 34
partial_gnatpp -P default.gpr -S src/single_task_declaration.adb -SL 35 -EL 38 -EC 11

echo ""
echo "-----------------------------------------------------------------"
echo "       Single task declarations package testing"
partial_gnatpp -P default.gpr -S src/single_task_decl_pckg.ads -SL 32 -EL 33
partial_gnatpp -P default.gpr -S src/single_task_decl_pckg.ads -SL 34 -EL 37
partial_gnatpp -P default.gpr -S src/single_task_decl_pckg.ads -SL 38 -EL 42
partial_gnatpp -P default.gpr -S src/single_task_decl_pckg.ads -SL 43 -EL 44

partial_gnatpp -P default.gpr -S src/single_task_decl_pckg.adb -SL 34 -EL 40












