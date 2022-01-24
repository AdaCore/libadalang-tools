rm -rf actual

gnatpp\
    --indentation=2\
    --max-line-length=110\
    --output-dir=actual for_loop_spec.adb

gnatpp\
    --indentation=2\
    --max-line-length=110\
    --output-dir=actual test_comment.adb

diff -r expected actual
