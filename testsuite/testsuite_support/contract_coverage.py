import os
import os.path
import re

from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors,
)


class ContractCoverageDriver(BaseDriver):
    """
    Driver to test the "contract_coverage" tool.

    In order to use it, just put *.adb and/or *.ads source files in the test
    directory. The output of "contract_coverage [FILES]" (FILES being the
    sorted list of these source files) will be checked against the expected
    output (test.out).
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
        if self.skip_test:
            self.result.set_status('DEAD', self.message)
            return

        source_files = sorted(
            filename
            for filename in os.listdir(self.test_dir)
            if re.match(r'.*\.ad[bs]', filename)
        )
        self.call_and_check(['contract_coverage'] + source_files)
        if self.diff:
            for diff_pair in self.diff:
                self.call(['diff', '-r'] + diff_pair.split() +
                          self.diff_args.split())
