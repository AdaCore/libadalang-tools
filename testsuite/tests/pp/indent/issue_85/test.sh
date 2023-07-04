echo "test.ads"
indent -P default.gpr -S test.ads -L 1 2 3 7 8 9 10 11 12 13
echo "test.adb"
indent -P default.gpr -S test.adb -L 1 2 8 10 11 13 15
echo "test_generic.ads"
indent -P default.gpr -S test_generic.ads -L 1 2 4
