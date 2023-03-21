#! /usr/bin/env python

"""
Usage::

    testsuite.py [OPTIONS]

Run the libadalang-tools testsuite.
"""

import os

import e3.testsuite
from e3.fs import sync_tree
from e3.os.fs import which
from e3.os.process import Run

from drivers.python_script import PythonScriptDriver
from drivers.shell_script import ShellScriptDriver


class Testsuite(e3.testsuite.Testsuite):
    tests_subdir = "tests"
    test_driver_map = {
        "python_script": PythonScriptDriver,
        "shell_script": ShellScriptDriver,
    }

    def add_options(self, parser):
        parser.add_argument(
            "--no-wip",
            action="store_true",
            help="Do not run tests for work-in-progress (WIP) programs",
        )
        parser.add_argument(
            "--fold-casing",
            action="store_true",
            help="Ignore casing in testcase outputs",
        )
        parser.add_argument(
            "--valgrind", action="store_true", help="Run tests under valgrind"
        )
        parser.add_argument(
            "--rewrite",
            "-r",
            action="store_true",
            help="Rewrite test baselines according to current output.",
        )
        parser.add_argument(
            "--tgen-rts-dir",
            action="store",
            help="alternative path to the source of the TGen_RTS project"
        )

    def set_up(self):
        super().set_up()

        self.env.no_wip = self.main.args.no_wip
        self.env.fold_casing = self.main.args.fold_casing
        self.env.valgrind = self.main.args.valgrind
        self.env.rewrite_baselines = self.main.args.rewrite

        # We need to add "." to the PATH, because some tests run programs in
        # the current directory.
        os.environ["PATH"] = "%s:." % os.environ["PATH"]

        # Put the testsuite drivers in the PATH
        script_dir = os.path.dirname(__file__)
        os.environ["PATH"] = "{}{}{}".format(
            os.path.abspath(os.path.join(script_dir, "ada_drivers", "bin")),
            os.pathsep,
            os.environ["PATH"],
        )

        if self.env.valgrind:
            # The --valgrind switch was given. Set the PATH to point to the
            # valgrind directory (see ../../valgrind/README).
            valgrind_dir = os.path.abspath(
                os.path.join(script_dir, "..", "..", "valgrind")
            )
            os.environ["PATH"] = valgrind_dir + os.pathsep + os.environ["PATH"]

        # Turn on strict mode for gnattest to catch real errors
        os.environ["GNATTEST_STRICT"] = "TRUE"

        # Copy, build, install and setup the TGen_RTS

        # First, locate the rts sources and copy them in the working dir
        if self.main.args.tgen_rts_dir:
            if os.path.isabs (self.main.args.tgen_rts_dir):
                rts_src_dir = self.main.args.tgen_rts_dir
            else:
                rts_src_dir = os.path.join(
                    os.getcwd(),
                    self.main.args.tgen_rts_dir
                )
        else:
            rts_src_dir = os.path.join(
                os.path.dirname(which("gnattest")),
                "..",
                "share",
                "tgen",
                "tgen_rts"
            )
        rts_working_dir = os.path.join(self.env.working_dir, "tgen_rts_src")
        sync_tree(rts_src_dir, rts_working_dir)

        # Build and install the RTS,
        p = Run (["gprbuild", "-Ptgen_rts.gpr"], cwd=rts_working_dir)
        if p.status == 0:
            p = Run(
                ["gprinstall",
                 "-Ptgen_rts.gpr",
                 "-p",
                 "--prefix=local"],
                 cwd=rts_working_dir
            )
        else:
            print ("failed to build tgen_rts")

        # Make it available through the GPR_PROJECT_PATH env var
        if p.status == 0:
            new_GPP = os.path.join(rts_working_dir, "local", "share", "gpr")
            if "GPR_PROJECT_PATH" in os.environ:
                new_GPP += os.pathsep + os.environ["GPR_PROJECT_PATH"]
            os.environ["GPR_PROJECT_PATH"] = new_GPP
        else:
            print ("failed to install tgen_rts")


if __name__ == "__main__":
    Testsuite(os.path.dirname(__file__)).testsuite_main()
