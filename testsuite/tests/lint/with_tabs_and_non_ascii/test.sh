rm -rf output
mkdir output
cp source/* output
cd output
gnatrefactor array_aggregates --sources my_package-child_package.adb
cd ..
diff expected/my_package-child_package.adb output/my_package-child_package.adb --strip-trailing-cr
