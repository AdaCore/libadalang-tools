gnattest -P simple -q
gprbuild -q -P h/test_driver.gpr
echo "line 2"
h/test_runner --routines=pkg.ads:2
echo "line 22"
h/test_runner --routines=pkg.ads:22
