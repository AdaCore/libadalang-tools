from gnatpython.testsuite import Testsuite as BaseTestsuite

from testsuite_support.contract_coverage import ContractCoverageDriver
from testsuite_support.python_script import PythonScriptDriver
from testsuite_support.shell_script import ShellScriptDriver


class Testsuite(BaseTestsuite):
    TEST_SUBDIR = 'tests'
    DRIVERS = {
        'contract_coverage': ContractCoverageDriver,
        'python_script': PythonScriptDriver,
        'shell_script': ShellScriptDriver,
    }
