import os

from gnatpython.testsuite import Testsuite as BaseTestsuite

import testsuite_support.contract_coverage


class Testsuite(BaseTestsuite):
    TEST_SUBDIR = 'tests'
    DRIVERS = {
        'contract_coverage':
            testsuite_support.contract_coverage.ContractCoverageDriver,
    }
