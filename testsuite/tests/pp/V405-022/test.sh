rm -rf actual

gnatpp\
    --keyword-upper-case\
    --output-dir=actual test.adb

gnatpp\
    --keyword-upper-case\
    --vertical-array-types\
    --vertical-named-aggregates\
    --output-dir=actual test_bis.adb

diff -r expected actual
