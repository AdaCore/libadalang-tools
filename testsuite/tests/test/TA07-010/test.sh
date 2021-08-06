ln -s nest1/nest2/nest3 sim
gnattest -P sim/simple.gpr -q --harness-dir=.
gprbuild -P sim/test_driver.gpr -q
cat sim/test_simple.gpr
cat sim/test_driver.gpr

