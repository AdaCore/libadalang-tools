import os

from typing import Union

from e3.fs import sync_tree
from e3.testsuite.control import YAMLTestControlCreator
from e3.testsuite.driver.classic import TestAbortWithError, TestSkip
from e3.testsuite.driver.diff import (
    DiffTestDriver,
    OutputRefiner,
    ReplacePath,
    Substitute,
)


class ToLower(OutputRefiner):
    """Output refiner to switch to lower case."""

    def refine(self, output):
        return output.lower()


class BaseDriver(DiffTestDriver):
    """Base class to provide common test driver helpers."""

    # Name of test directories for tools that are considered work in progress
    WIP_TOOLS = set()

    # Name of test directories for all tools
    ALL_TOOLS = {
        "pp",
        "metric",
        "stub",
        "test",
        "laltools",
    } | WIP_TOOLS

    @property
    def tool_dirname(self):
        """
        Return the name of the per-tool directory for this testcase.

        We are looking for any directory in the path whose name matches the
        name of a tool (see ALL_TOOLS above).

        :rtype: str
        """
        for name in self.test_env["test_name"].split("__"):
            if name in self.ALL_TOOLS:
                return name
        raise TestAbortWithError("Cannot guess which tool is tested from test name")

    @property
    def baseline(self):
        filename, is_regexp = self.baseline_file
        filename = self.test_dir(filename)
        baseline: Union[str, bytes]

        try:
            if self.default_encoding == "binary":
                with open(filename, "rb") as text_f:
                    baseline = text_f.read()
            else:
                with open(filename, "r", encoding=self.default_encoding) as bin_f:
                    baseline = bin_f.read()
        except FileNotFoundError:
            # Allow a missing test baseline file - treat as empty
            return (None, "", is_regexp)
        except Exception as exc:
            raise TestAbortWithError(
                "cannot read baseline file ({}: {})".format(type(exc).__name__, exc)
            )

        return (filename, baseline, is_regexp)

    @property
    def test_control_creator(self):
        return YAMLTestControlCreator(
            {
                "windows": self.env.target.os.name == "windows",
                "x86": self.env.target.cpu.bits == 32,
            }
        )

    def shared_dir(self, *name):
        """Full path the the shared resource. If name starts with "internal",
           the resource is looked up from the internal testsuite dir.

           :param name: Parameters as passed to `os.path.join`, excluding the
                first one if it is equal to "internal".
        """
        if len(name) >= 1 and name[0] == "internal":
            return os.path.join(
                self.env.root_dir,
                "tests",
                "internal",
                "shared",
                *name[1:]
            )
        else:
            return os.path.join(self.env.root_dir, "shared", *name)

    def sync_res(self, name, dest=None):
        """Sync resources to working directory.

        :param name: Name of the resource (path relative to SHARED_DIR).
            prefix with "internal/" to lookup the shared resource in the
            internal testsuite shared dir instead of the top level one.
        :param dest: Destination. If None, the resource is synchronized
            into the working directory.
        """

        dest = self.working_dir() if dest is None else self.working_dir(dest)

        try:
            sync_tree(self.shared_dir(*name.split('/')), dest, delete=False)
        except Exception as exc:
            raise TestAbortWithError(
                f"error during copy of shared resource {name}: {exc}"
            )

    def set_up(self):
        super().set_up()

        # If we must not test the tool this testcase exercize, just skip it
        if self.env.no_wip and self.tool_dirname in self.WIP_TOOLS:
            raise TestSkip("WIP tool")

        if "description" not in self.test_env:
            raise TestAbortWithError('test.yaml: missing "description" field')

        # Load any resources the testcase may request into the workspace.
        if "resources" in self.test_env:
            for src_dir, dest_dir in self.test_env["resources"].items():
                self.sync_res(src_dir, dest_dir)

    @property
    def output_refiners(self):
        result = super().output_refiners + [ReplacePath(self.working_dir())]
        if self.env.fold_casing:
            result.append(ToLower())
        if self.test_env.get("canonicalize_backslashes", False):
            result.append(Substitute("\\", "/"))
        return result
