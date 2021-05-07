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

diff -r expected actual
