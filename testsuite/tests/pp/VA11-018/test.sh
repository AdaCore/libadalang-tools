rm -rf actual

gnatpp --output-dir=actual -P default.gpr parent.ads

diff -r expected actual

