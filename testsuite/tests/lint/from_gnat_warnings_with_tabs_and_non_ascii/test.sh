rm -rf output
mkdir output
cp source/* output
cp gnat_log output
cd output
gnatrefactor array_aggregates --from-gnat-warnings gnat_log
cd ..
diff expected/my_package-child_package.adb output/my_package-child_package.adb --strip-trailing-cr
