rm -rf actual
gnatpp\
    --call-threshold=1\
    --output-dir=actual test.ads test1.ads test2.ads
diff -r actual expected
