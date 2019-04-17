import os
import sys

import testsuite_support
from testsuite_support.base_driver import BaseDriver, catch_test_errors


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
    def run(self):
        if self.skip_test:
            self.result.set_status('DEAD', self.message)
            return

        # Run the Python script, making testsuite_support modules available
        testsuite_support_path = os.path.dirname(
            os.path.dirname(testsuite_support.__file__))

        env = dict(os.environ)
        old_path = env.get('PYTHONPATH', '')
        if old_path:
            new_path = '{}{}{}'.format(
                testsuite_support_path, os.path.pathsep, old_path)
        else:
            new_path = testsuite_support_path
        env['PYTHONPATH'] = new_path

        self.call_and_check([sys.executable, 'test.py'], env=env)
