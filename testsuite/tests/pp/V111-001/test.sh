rm -rf actual

gnatpp\
    --indentation=2\
    --max_line-length=110\
    --output-dir=actual for_loop_spec.adb

diff -r expected actual
