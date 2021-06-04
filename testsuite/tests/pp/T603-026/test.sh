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
    --based-grouping=4\
    --comments-gnat-indentation\
    --indentation=3\
    --indent-continuation=2\
    --decimal-grouping=3\
    --max-line-length=132\
    --separate-loop-then\
    --use-on-new-line\
    --split-line-before-op\
    --indent-named-statements\
    --par_threshold=0\
    --call_threshold=0\
    --vertical-enum-types\
    --vertical-array-types\
    --vertical-named-aggregates\
    --vertical-case-alternatives\
    --preserve-line-breaks\
    --eol=unix\
    --output-dir=actual preserve_line_breaks.adb

diff -r expected actual
