rm -rf out out.sep out.no-sep

gnatpp separate_thens.adb --max-line-length=40 --output-dir=out
gnatpp separate_thens.adb --max-line-length=40 --separate-loop-then --output-dir=out.sep
gnatpp separate_thens.adb --max-line-length=40 --no-separate-loop-then --output-dir=out.no-sep

echo out
cd out
gcc -c separate_thens.adb -gnatyi
cd ..

echo out.sep
cd out.sep
gcc -c separate_thens.adb -gnatyi
cd ..

echo out.no-sep
cd out.no-sep
gcc -c separate_thens.adb -gnatyi
cd ..
