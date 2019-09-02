import os

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

    def add_options(self):
        self.main.add_option(
            '--no-wip', action='store_true', default=False,
            help='Do not run tests for work-in-progress (WIP) programs')
        self.main.add_option(
            '--fold-casing', action='store_true', default=False,
            help='Ignore casing in testcase outputs')
        self.main.add_option(
            '--valgrind', action='store_true', default=False,
            help='Run tests under valgrind')

    def tear_up(self):
        if self.global_env['options'].valgrind:
            # The --valgrind switch was given. Set the PATH to point to the
            # valgrind directory (see ../../valgrind/README).
            script_dir = os.path.dirname(__file__)
            valgrind_dir = os.path.abspath(
                os.path.join(script_dir, '..', '..', 'valgrind'))
            os.environ["PATH"] = valgrind_dir + os.pathsep + os.environ["PATH"]
