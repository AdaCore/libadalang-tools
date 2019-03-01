rm -rf out
gnatpp -q --syntax-only --source-line-breaks --output-dir=out in/*.ad?
diff -r in out
