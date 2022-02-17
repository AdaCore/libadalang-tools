rm -rf actual
gnatpp --output-dir=actual test.ads
diff -r actual expected
