import sys
import os

from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors,
)


class ShellScriptDriver(BaseDriver):
    """
    Driver to run a sh script.

    Interface:

    * put a "test.sh" script in the test directory;
    * put a "test.out" text file in the test directory,
      with expected results. A missing test.out is treated
      as empty.

    This driver will run the sh script. Its output is then checked against
    the expected output (test.out file). Use this driver only for legacy tests.
    """

    TIMEOUT = 300

    #
    # Driver entry points
    #

    @catch_test_errors
    def tear_up(self):
        super(ShellScriptDriver, self).tear_up()

        args = self.test_env.get('args', None)

        if args:
            self.args = args.split()
        self.diff = self.test_env.get('diff', None)
        self.diff_args = self.test_env.get('diff_args', '')

    @catch_test_errors
    def run(self):
        # We need to add "." to the PATH, because some tests run programs in
        # the current directory.
        os.environ['PATH'] = "%s:." % os.environ['PATH']
        os.chmod(os.path.join(self.working_dir(), 'test.sh'), 0755)
        self.call_and_check(['sh', './test.sh'])
        if self.diff:
            for diff_pair in self.diff:
                self.call(['diff', '-r'] + diff_pair.split() +
                          self.diff_args.split())
