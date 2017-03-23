gnatpp -v src/*.ad? --output-dir=actual --name_mixed_case --based-grouping=4 --decimal-grouping=3
diff -r expected actual | head
