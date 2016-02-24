import os
import os.path
import re

from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors, SetupError,
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
    def run(self):
        source_files = sorted(
            filename
            for filename in os.listdir(self.test_dir)
            if re.match(r'.*\.ad[bs]', filename)
        )
        self.run_and_check(['contract_coverage'] + source_files)
