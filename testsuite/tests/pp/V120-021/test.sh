rm -rf actual

gnatpp\
    --max-line-length=80\
    --indentation=2\
    --output-dir=actual test.adb

diff -r expected actual
