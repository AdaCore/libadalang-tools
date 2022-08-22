from typing import Union

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
        "check",
        "laltools",
        "refactoring_auto_import",
        "refactoring_add_parameter",
        "refactoring_introduce_parameter",
        "refactoring_remove_parameter",
        "refactoring_change_parameters_type",
        "refactoring_change_parameters_default_value",
        "refactoring_safe_rename",
        "refactoring_extract_subprogram",
        "refactoring_pull_up_declaration",
        "refactoring_suppress_separate",
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
        raise TestAbortWithError(
            "Cannot guess which tool is tested from test name"
        )

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
                with open(
                    filename, "r", encoding=self.default_encoding
                ) as bin_f:
                    baseline = bin_f.read()
        except FileNotFoundError:
            # Allow a missing test baseline file - treat as empty
            return (None, "", is_regexp)
        except Exception as exc:
            raise TestAbortWithError(
                "cannot read baseline file ({}: {})".format(
                    type(exc).__name__, exc
                )
            )

        return (filename, baseline, is_regexp)

    @property
    def test_control_creator(self):
        return YAMLTestControlCreator(
            {"windows": self.env.target.os.name == "windows"}
        )

    def set_up(self):
        super().set_up()

        # If we must not test the tool this testcase exercize, just skip it
        if self.env.no_wip and self.tool_dirname in self.WIP_TOOLS:
            raise TestSkip("WIP tool")

        if "description" not in self.test_env:
            raise TestAbortWithError('test.yaml: missing "description" field')

    @property
    def output_refiners(self):
        result = super().output_refiners + [ReplacePath(self.working_dir())]
        if self.env.fold_casing:
            result.append(ToLower())
        if self.test_env.get("canonicalize_backslashes", False):
            result.append(Substitute("\\", "/"))
        return result
