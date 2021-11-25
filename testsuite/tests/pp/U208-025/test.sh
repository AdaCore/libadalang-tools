rm -rf actual


gnatpp\
    --max-line-length=119\
    --output-dir=actual simple.adb

diff -r expected actual
