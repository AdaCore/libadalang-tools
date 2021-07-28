rm -rf actual


gnatpp\
    --alignment\
    --name-case-as-declared\
    --attribute-mixed-case\
    --keyword-lower-case\
    --comments-gnat-beginning\
    --comments-fill\
    --indentation=3\
    --indent-continuation=2\
    --max-line-length=99\
    --par_threshold=0\
    --output-dir=actual expr_function_is.adb

diff -r expected actual
