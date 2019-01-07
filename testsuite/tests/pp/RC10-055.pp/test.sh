rm -rf actual
gnatpp --quiet --output-dir=actual lf.ads crlf.ads mixed.ads
diff -r expected actual
