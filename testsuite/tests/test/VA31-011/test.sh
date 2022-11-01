gnattest simple.gpr -q --tests-dir=tests --validate-type-extensions
gprbuild -P gnattest/harness/test_driver.gpr -q
gnattest/harness/test_runner
