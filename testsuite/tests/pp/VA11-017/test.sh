rm -rf actual

gnatpp --output-dir=actual my_package.ads

diff -r expected actual

