gnattest simple.gpr p.ads --skeleton-default=pass --passed-tests=hide -q
gprbuild -P gnattest/harness/test_driver.gpr -q
gnattest/harness/test_runner
