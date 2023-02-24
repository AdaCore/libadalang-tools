gnatmake -q -f -g -O0 -gnata normalize.adb
normalize preserve_existing_continuation_lines.adb > normalized_preserve_existing_continuation_lines.adb

gnatpp\
    --layout=default\
    --source-line-breaks\
    -M132\
    -cl3\
    -nM\
    --eol=unix\
    --pipe\
   normalized_preserve_existing_continuation_lines.adb
