rm -rf output
mkdir output
cp -a test.gpr main.adb output
gnatrefactor array_aggregates -P output/test.gpr
diff expected output --strip-trailing-cr