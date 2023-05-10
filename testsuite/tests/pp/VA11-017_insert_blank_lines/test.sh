rm -rf actual

gnatpp --output-dir=actual -P default.gpr my_package.ads

diff -r expected actual

