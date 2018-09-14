rm -f out test
gnatstub --subunits pack.adb --output-dir=out
gnatstub --subunits pack-child.adb --output-dir=out
mkdir test
cp -p *.ad? out/*.ad? test
cd test
gprbuild -q -f -gnata -gnatws main.adb
main
