rm -rf actual 

gnatpp --use-tabs --output-dir=actual test.adb
gnatpp --output-dir=actual test_bis.adb

diff -r expected actual
