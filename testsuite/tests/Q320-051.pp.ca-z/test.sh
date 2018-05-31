gnatpp -q -P proj.gpr --output-dir=actual --name_mixed_case --based-grouping=4 --decimal-grouping=3 --comments-fill
diff -r expected actual | head -400
