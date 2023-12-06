rm -rf out
gnatpp -W8 -q --syntax-only --source-line-breaks --output-dir=out in/*.ad?
diff -r in out
