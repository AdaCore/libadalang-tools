rm -rf actual

# The used switches
#            "--alignment", DEFAULT
#            "--attribute-mixed-case", DEFAULT
#            "--par_threshold=0"


gnatpp\
    --alignment\
    --attribute-mixed-case\
    --par_threshold=0\
    --output-dir=actual pp_off_test.ads

gnatpp\
    --alignment\
    --attribute-mixed-case\
    --par_threshold=0\
    --pp-off="!custom pp off"\
    --pp-on="!custom pp on"\
    --output-dir=actual custom_pp_off_on_test.ads

gnatpp\
    --alignment\
    --attribute-mixed-case\
    --par_threshold=0\
    --preserve-blank-lines\
    --output-dir=actual pp_off_on_test.ads

diff -r expected actual
