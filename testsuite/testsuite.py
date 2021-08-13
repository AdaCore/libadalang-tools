#! /usr/bin/env python

"""
Usage::

    testsuite.py [OPTIONS]

Run the libadalang-tools testsuite.
"""

import os

import e3.testsuite

from drivers.python_script import PythonScriptDriver
from drivers.shell_script import ShellScriptDriver


class Testsuite(e3.testsuite.Testsuite):
    tests_subdir = 'tests'
    test_driver_map = {
        'python_script': PythonScriptDriver,
        'shell_script': ShellScriptDriver,
    }

    def add_options(self, parser):
        parser.add_argument(
            '--no-wip', action='store_true',
            help='Do not run tests for work-in-progress (WIP) programs')
        parser.add_argument('--fold-casing', action='store_true',
                            help='Ignore casing in testcase outputs')
        parser.add_argument('--valgrind', action='store_true',
                            help='Run tests under valgrind')
        parser.add_argument(
            '--rewrite', '-r', action='store_true',
            help='Rewrite test baselines according to current output.'
        )

    def set_up(self):
        super().set_up()

        self.env.no_wip = self.main.args.no_wip
        self.env.fold_casing = self.main.args.fold_casing
        self.env.valgrind = self.main.args.valgrind
        self.env.rewrite_baselines = self.main.args.rewrite

        # We need to add "." to the PATH, because some tests run programs in
        # the current directory.
        os.environ['PATH'] = "%s:." % os.environ['PATH']

        # Put the testsuite drivers in the PATH
        script_dir = os.path.dirname(__file__)
        os.environ["PATH"] = "{}{}{}".format(
            os.path.abspath(os.path.join(script_dir, 'ada_drivers', 'bin')),
            os.pathsep,
            os.environ["PATH"])

        if self.env.valgrind:
            # The --valgrind switch was given. Set the PATH to point to the
            # valgrind directory (see ../../valgrind/README).
            valgrind_dir = os.path.abspath(
                os.path.join(script_dir, '..', '..', 'valgrind'))
            os.environ["PATH"] = valgrind_dir + os.pathsep + os.environ["PATH"]

        # Turn on strict mode for gnattest to catch real errors
        os.environ["GNATTEST_STRICT"] = "TRUE"

if __name__ == '__main__':
    Testsuite(os.path.dirname(__file__)).testsuite_main()
