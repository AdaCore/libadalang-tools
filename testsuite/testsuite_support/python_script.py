import sys

from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors,
)


class PythonScriptDriver(BaseDriver):
    """
    Driver to run a Python script.

    Interface:

    * put a "test.py" script in the test directory;
    * put a "test.out" text file in the test directory.

    This driver will run the Python script. Its output is then checked against
    the expected output (test.out file). This mechanism is the most flexible
    way to write a testcase, but also the more verbose one and the most complex
    one. Use this driver when no other one fits.
    """

    TIMEOUT = 300

    #
    # Driver entry points
    #

    @catch_test_errors
    def tear_up(self):
        super(ContractCoverageDriver, self).tear_up()

        args = self.test_env.get('args', None)

        if args:
            self.args = args.split()
        self.diff = self.test_env.get('diff', None)
        self.diff_args = self.test_env.get('diff_args', '')

    @catch_test_errors
    def run(self):
        self.call_and_check([sys.executable, 'test.py'])
        if self.diff:
            for diff_pair in self.diff:
                self.call(['diff', '-r'] + diff_pair.split() +
                          self.diff_args.split())
