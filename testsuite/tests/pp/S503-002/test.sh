rm -rf actual

gnatpp\
    --alignment\
    --par-threshold=0\
    --vertical-enum-types\
    --output-dir=actual wrong_indent.ads

diff -r expected actual
