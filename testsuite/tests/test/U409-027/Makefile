.PHONY: all

all:
	@gnattest -P p
	@gprbuild -P gnattest/harness/test_driver.gpr -q
	@-gnattest/harness/test_runner > foo\
	&& echo "exit code from gpr not set" || echo -n ""
