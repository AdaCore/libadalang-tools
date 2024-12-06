#! /usr/bin/env python

"""
Usage::

    testsuite.py [OPTIONS]

Run the libadalang-tools testsuite.
"""

import os

import e3.testsuite
from e3.fs import sync_tree
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
            "--setup-tgen-rts",
            action="store_true",
            help="Build and install TGen_RTS to prevent tests from re-building"
                 " the library. This is not needed if the environment already"
                 " provides the installed project."
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

        if self.main.args.setup_tgen_rts:
            # Copy the TGen_RTS sources from the tree to a temporary directory
            rts_build_dir = os.path.join(self.working_dir, "tgen_rts")
            rts_install_dir = os.path.join(rts_build_dir, "local")
            sync_tree(
                os.path.join(self.root_dir, "..", "src", "tgen", "tgen_rts"),
                rts_build_dir
            )
            e3.testsuite.logger.info("Building TGen_RTS ...")
            p_build = Run([
                "gprbuild",
                f"-P{os.path.join(rts_build_dir, 'tgen_rts.gpr')}",
                "-g",
                "-bargs",
                "-Es"
            ])
            if p_build.status != 0:
                e3.testsuite.logger.fatal(
                    f"Failed to build TGen_RTS: {p_build.out}"
                )
                exit(1)

            e3.testsuite.logger.info("Installing TGen_RTS ...")
            p_install = Run([
                "gprinstall",
                "-p",
                f"-P{os.path.join(rts_build_dir, 'tgen_rts.gpr')}",
                f"--prefix={rts_install_dir}",
            ])
            if p_install.status != 0:
                e3.testsuite.logger.fatal(
                    f"Failed to install TGen_RTS: {p_install.out}"
                )
                exit(1)
            self.env.add_search_path(
                "GPR_PROJECT_PATH",
                os.path.join(rts_install_dir, "share", "gpr")
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

        # Set a fixed seed for TGen random generation, in order to keep the
        # testsuite deterministic.
        os.environ["TGEN_RANDOM_SEED"] = "1234"


if __name__ == "__main__":
    Testsuite(os.path.dirname(__file__)).testsuite_main()
