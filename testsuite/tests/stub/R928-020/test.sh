rm -rf src
mkdir src
cp -p source.ada src
cd src
gnatchop -q -w source.ada
cd ..

echo dir with full path, wrong
gnatstub -q --update-body=2 -Pdefault.gpr /
cat src/aaa.ads src/aaa.adb

echo file and dir with full path, wrong
gnatstub -q --update-body=2 -Pdefault.gpr src/aaa.ads /
cat src/aaa.ads src/aaa.adb

echo dir with relative path, wrong
gnatstub -q --update-body=2 -Pdefault.gpr src
cat src/aaa.ads src/aaa.adb

echo file and dir with relative path, acceptable
gnatstub -q --update-body=2 -Pdefault.gpr src/aaa.ads src
cat src/aaa.ads src/aaa.adb

rm -rf src
mkdir src
cp -p source.ada src
cd src
gnatchop -q -w source.ada
cd ..

echo no dir, right
gnatstub -q --update-body=2 -Pdefault.gpr src/aaa.ads
cat src/aaa.ads src/aaa.adb
