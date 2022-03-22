rm -rf actual

gnatpp --output-dir=actual examples.ads

gnatpp\
    --alignment\
    --attribute-mixed-case\
    --based-grouping=4\
    --call_threshold=4\
    --comments-gnat-beginning\
    --decimal-grouping=3\
    --enum-case-as-declared\
    --indent-continuation=2\
    --indentation=3\
    --keyword-lower-case\
    --max-line-length=99\
    --name-case-as-declared\
    --no-compact\
    --number-case-as-declared\
    --par_threshold=0\
    --pragma-mixed-case\
    --split-line-before-op\
    --split-line-before-record\
    --type-case-as-declared\
    --output-dir=actual examples_bis.ads

diff -r expected actual
