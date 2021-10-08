rm -rf actual

gnatpp\
    --indent-continuation=3\
    --output-dir=actual wrong_indent.adb

diff -r expected actual
