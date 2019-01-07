rm -rf actual
gnatstub --quiet --output-dir=actual lf.ads crlf.ads
diff -r expected actual
