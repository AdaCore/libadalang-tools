#Testing the support of Ada_No_Type_Object_Renaming_Decl

rm -rf actual

gnatpp --output-dir=actual test.ads
gnatpp --output-dir=actual main.adb

diff -r expected actual
