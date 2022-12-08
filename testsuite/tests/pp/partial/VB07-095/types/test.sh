partial_gnatpp -P default.gpr -S main.adb -SL 1 -SC 4 -EL 10 -EC 4

echo "-----------------------------------------------------------------"
echo "       Type declarations testing"
echo "    **   Full type declarations"
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 35 -EL 36
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 36 -EL 37
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 37 -EL 38
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 39 -EL 40
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 40 -EL 41

partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 42 -EL 47
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 48 -EL 51
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 51 -EL 52

echo "" 
echo "    **   Incomplete type declarations: Recursive type example"
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 58 -EL 59
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 59 -EL 60

partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 57 -EL 60 --source-line-breaks

partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 61 -EL 66
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 67 -EL 68
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 68 -EL 69

echo ""
echo "    **   Incomplete type declarations: Mutually dependent acces type example"
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 73 -EL 74
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 74 -EL 75
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 76 -EL 77
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 77 -EL 78
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 84 -EL 94
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 95 -EL 96
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 96 -EL 97

echo ""
echo "    **   Private type declarations"
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 102 -EL 103
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 103 -EL 104

echo ""
echo "    **   Private extension declarations"
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 109 -EL 110

echo ""
echo "    **   Task type declarations"
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 115 -EL 119
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 119 -EL 124

echo ""
echo "    **   Single task declarations"
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 129 -EL 132
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 133 -EL 137
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 138 -EL 139

partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 141 -EL 142
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 142 -EL 143
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 143 -EL 144

partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 140 -EL 144 --source-line-breaks

partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 146 -EL 147
partial_gnatpp -P default.gpr -S src/type_declarations.ads -SL 147 -EL 148

echo ""
echo "-----------------------------------------------------------------"
echo "       Task type declarations"
partial_gnatpp -P default.gpr -S src/task_type_arr.adb -SL 30 -EL 33
partial_gnatpp -P default.gpr -S src/task_type_arr.adb -SL 34 -EL 43
partial_gnatpp -P default.gpr -S src/task_type_arr.adb -SL 44 -EL 44 -EC 37

echo ""
echo "-----------------------------------------------------------------"
echo "       Simple task type declarations"
partial_gnatpp -P default.gpr -S src/simple_task_type_decl.adb -SL 29 -EL 30
partial_gnatpp -P default.gpr -S src/simple_task_type_decl.adb -SL 31 -EL 35
partial_gnatpp -P default.gpr -S src/simple_task_type_decl.adb -SL 36 -EL 37

echo ""
echo "-----------------------------------------------------------------"
echo "       Protected type declarations"
partial_gnatpp -P default.gpr -S src/protected_obj_type_decl.adb -SL 31 -EL 37
partial_gnatpp -P default.gpr -S src/protected_obj_type_decl.adb -SL 38 -EL 49
partial_gnatpp -P default.gpr -S src/protected_obj_type_decl.adb -SL 50 -EL 50 -EC 22

echo ""
echo "-----------------------------------------------------------------"
echo "       Protected object entries"
partial_gnatpp -P default.gpr -S src/protected_objects_entries.adb -SL 30 -EL 37
partial_gnatpp -P default.gpr -S src/protected_objects_entries.adb -SL 38 -EL 58

partial_gnatpp -P default.gpr -S src/protected_objects_entries.adb -SL 45 -EL 57

partial_gnatpp -P default.gpr -S src/protected_objects_entries.adb -SL 61 -EL 62
partial_gnatpp -P default.gpr -S src/protected_objects_entries.adb -SL 63 -EL 70 -EC 11


