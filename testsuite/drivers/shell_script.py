import os

from drivers.base_driver import BaseDriver


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

    @property
    def default_process_timeout(self):
        result = 300

        # Tests run almost 40 times slower under valgrind, so increase the
        # TIMEOUT in that case.
        if self.env.valgrind:
            result *= 40

        return result

    def run(self):
        # Some tests expect the script to stop with an error status code: in
        # that case just print it so that the baseline catches it.
        p = self.shell(['bash', 'test.sh'], catch_error=False)
        if p.status:
            self.output += (
                ">>>program returned status code {}\n".format(p.status)
            )
