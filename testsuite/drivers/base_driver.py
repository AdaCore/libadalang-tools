import difflib
import os
import os.path

from e3.testsuite.control import YAMLTestControlCreator
from e3.testsuite.driver.classic import TestAbortWithError, TestSkip
from e3.testsuite.driver.diff import (DiffTestDriver, OutputRefiner,
                                      ReplacePath, Substitute)


class ToLower(OutputRefiner):
    """Output refiner to switch to lower case."""

    def refine(self, output):
        return output.lower()


class BaseDriver(DiffTestDriver):
    """Base class to provide common test driver helpers."""

    # Name of test directories for tools that are considered work in progress
    WIP_TOOLS = {'test'}

    # Name of test directories for all tools
    ALL_TOOLS = {'pp', 'metric', 'stub', 'laltools'} | WIP_TOOLS

    @property
    def tool_dirname(self):
        """
        Return the name of the per-tool directory for this testcase.

        We are looking for any directory in the path whose name matches the
        name of a tool (see ALL_TOOLS above).

        :rtype: str
        """
        for name in self.test_env['test_name'].split('__'):
            if name in self.ALL_TOOLS:
                return name
        raise TestAbortWithError(
            'Cannot guess which tool is tested from test name'
        )

    @property
    def baseline(self):
        # Allow a missing test.out -- treat as empty
        test_out = self.test_dir("test.out")
        if os.path.exists(test_out):
            with open(test_out, encoding=self.default_encoding) as f:
                baseline = f.read()
        else:
            baseline = ''

        return (None, baseline, False)

    @property
    def test_control_creator(self):
        return YAMLTestControlCreator(
            {"windows": self.env.target.os.name == "windows"}
        )

    def set_up(self):
        super().set_up()

        # If we must not test the tool this testcase exercize, just skip it
        if (
            self.env.no_wip and
            self.tool_dirname in self.WIP_TOOLS
        ):
            raise TestSkip("WIP tool")

        if 'description' not in self.test_env:
            raise TestAbortWithError('test.yaml: missing "description" field')

    @property
    def output_refiners(self):
        result = super().output_refiners + [
            ReplacePath(self.working_dir()),
        ]
        if self.env.fold_casing:
            result.append(ToLower())
        if self.test_env.get('canonicalize_backslashes', False):
            result.append(Substitute("\\", "/"))
        return result
