
rm -rf actual

# The used switches
#    "--alignment", DEFAULT
#    "--align-modes", DEFAULT
#    "--name-case-as-declared", DEFAULT 
#    "--attribute-mixed-case", DEFAULT
#    "--keyword-lower-case", DEFAULT
#    "--enum-case-as-declared",
#    "--type-case-as-declared",
#    "--number-case-as-declared",
#    "--pragma-mixed-case",
#    "--based-grouping=4",
#    "--comments-gnat-indentation", DEFAULT
#    "--indentation=3",
#    "--indent-continuation=2", DEFAULT
#    "--decimal-grouping=3",
#    "--max-line-length=132",
#    "--separate-loop-then",
#    "--use-on-new-line",
#    "--insert-blank-lines",
#    "--split-line-before-op",
#    "--split-line-before-record",
#    "--indent-named-statements",
#    "--par_threshold=0",
#    "--call_threshold=0",
#    "--vertical-enum-types",
#    "--vertical-array-types",
#    "--vertical-named-aggregates",
#    "--vertical-case-alternatives",
#    "--eol=unix"


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
    --insert-blank-lines\
    --split-line-before-op\
    --split-line-before-record\
    --indent-named-statements\
    --par_threshold=0\
    --call_threshold=0\
    --vertical-enum-types\
    --vertical-array-types\
    --vertical-named-aggregates\
    --vertical-case-alternatives\
    --eol=unix\
    --output-dir=actual insert_blank_lines.adb

diff -r expected actual
