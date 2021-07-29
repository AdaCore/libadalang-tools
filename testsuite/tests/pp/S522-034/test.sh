rm -rf actual

gnatpp\
    --alignment\
    --vertical-enum-types\
    --output-dir=actual vert.ads

diff -r expected actual
