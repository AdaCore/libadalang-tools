rm -rf actual

gnatpp --split-line-before-record --output-dir=actual test.ads

diff -r expected actual
