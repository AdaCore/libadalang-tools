rm -rf actual

gnatpp\
    --alignment\
    --align-modes\
    --name-case-as-declared\
    --attribute-mixed-case\
    --keyword-lower-case\
    --enum-case-as-declared\
    --type-case-as-declared\
    --number-case-as-declared\
    --pragma-mixed-case\
    --indentation=3\
    --indent-continuation=2\
    --decimal-grouping=3\
    --max-line-length=132\
    --separate-loop-then\
    --split-line-before-op\
    --indent-named-statements\
    --par-threshold=1\
    --call-threshold=1\
    --vertical-enum-types\
    --vertical-array-types\
    --vertical-named-aggregates\
    --vertical-case-alternatives\
    --no-compact\
    --output-dir=actual wrong_indent.ads

diff -r expected actual
