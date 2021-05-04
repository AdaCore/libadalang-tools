rm -rf actual

# The used switches
#           "--alignment",
#           "--name-case-as-declared",
#           "--attribute-mixed-case",
#           "--keyword-lower-case",
#           "--enum-case-as-declared",
#           "--type-case-as-declared",
#           "--number-case-as-declared",
#           "--pragma-mixed-case",
#           "--based-grouping=4",
#           "--comments-gnat-beginning",
#           "--comments-fill", -- use "--  !pp off/on" for ASCII art
#           "--indentation=3",
#           "--indent-continuation=2",
#           "--decimal-grouping=3",
#           "--max-line-length=99",
#           "--split-line-before-record",
#           "--par_threshold=0",
#           "--call_threshold=4",
#           "--preserve-line-breaks",
#           "--insert-blank-lines",
#           "--eol=crlf"


gnatpp\
    --alignment\
    --name-case-as-declared\
    --attribute-mixed-case\
    --keyword-lower-case\
    --enum-case-as-declared\
    --type-case-as-declared\
    --number-case-as-declared\
    --pragma-mixed-case\
    --based-grouping=4\
    --comments-gnat-beginning\
    --comments-fill\
    --indentation=3\
    --indent-continuation=2\
    --decimal-grouping=3\
    --max-line-length=99\
    --split-line-before-record\
    --par_threshold=0\
    --call_threshold=4\
    --output-dir=actual par_threshold_is.adb

diff -r expected actual
