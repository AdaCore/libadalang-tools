gnatmake -q -f -g -O0 -gnata normalize.adb
normalize gnatpp_playground.adb > normalized_gnatpp_playground.adb
gnatpp \
    --pipe \
    --vertical-enum-types \
    --vertical-array-types \
    --vertical-named-aggregates \
    --vertical-case-alternatives \
    -M132 \
    -cl3 \
    --separate-loop-then \
    --use-on-new-line \
    --par_threshold=0 \
    --call_threshold=0 \
    --insert-blank-lines \
    --eol=unix \
    --based-grouping=3 \
    --decimal-grouping=3 \
    --split-line-before-op \
    \
    -nM \
  normalized_gnatpp_playground.adb
