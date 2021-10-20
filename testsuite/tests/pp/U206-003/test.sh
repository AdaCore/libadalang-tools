rm -rf actual


gnatpp\
    --par-threshold=0\
    --call-threshold=0\
    --separate-loop-then\
    --insert-blank-lines\
    --comments-gnat-beginning\
    --comments-gnat-indentation\
    --split-line-before-record\
    --no-compact\
    --vertical-named-aggregates\
    --output-dir=actual ramsim.ads

diff -r expected actual
