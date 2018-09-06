import difflib
import os
import os.path

from gnatpython import fileutils
from gnatpython.ex import Run, STDOUT
from gnatpython.testsuite.driver import TestDriver


CWD = os.path.dirname(os.path.abspath(__file__))


class SetupError(Exception):
    """Exception to raise when the testcase is invalid.

    Helper exception to work with catch_test_errors: see below.
    """
    pass


class TestError(Exception):
    """Exception to raise when the testcase fails.

    Helper exception to work with catch_test_errors: see below.
    """
    pass


def catch_test_errors(func):
    """
    Helper decorator for driver entry points.

    This returns a wrapper around func that catches SetupError and TestError
    exceptions and that turns them into the appropriate test status. Using
    exceptions is convenient to stop any method from any point: this simplifies
    the control flow.
    """

    def wrapper(self, *args, **kwargs):
        try:
            return func(self, *args, **kwargs)
        except SetupError as exc:
            self.set_setup_error(exc.message)
        except TestError as exc:
            self.set_failure(exc.message)
    return wrapper


class BaseDriver(TestDriver):
    """
    Base class to provide common test driver helpers.

    Ideally, these should end up in GNATpython, but this base class acts as a
    staging area: once it has been proven that some feature is useful, it may
    be easier to submit it upstream...
    """

    TIMEOUT = None

    def tear_up(self):
        super(BaseDriver, self).tear_up()
        self.create_test_workspace()

        # Do a case insensitive subst, because gnatpp does not yet generate the
        # correct case of identifiers. ???When that is fixed, change this.
        self.register_path_subst(self.working_dir().lower())

        if 'description' not in self.test_env:
            raise SetupError('test.yaml: missing "description" field')

        # Allow a missing test.out -- treat as empty.
        if os.path.isfile(os.path.join(self.test_dir, self.expected_file)):
            self.result.expected_output = self.read_file(self.expected_file)
        else:
            self.result.expected_output = ''

        # See if we expect a failure for this testcase
        try:
            comment = self.test_env['expect_failure']
        except KeyError:
            self.expect_failure = False
            self.expect_failure_comment = None
        else:
            # Because of wrapping in the YAML file, we can get multi-line
            # strings, which is not valid for comments.
            comment = comment.replace('\n', ' ').strip()

            self.expect_failure = True
            if not (comment is None or isinstance(comment, basestring)):
                raise SetupError('Invalid "expect_failure" entry:'
                                 ' expected a string but got {}'.format(
                                     type(comment)))
            self.expect_failure_comment = comment

    def read_file(self, filename):
        """Return the content of `filename`."""
        with open(filename, 'r') as f:
            return f.read()

    def set_setup_error(self, message):
        self.result.set_status('PROBLEM', message)

    def set_failure(self, message):
        if self.expect_failure:
            self.result.set_status('XFAIL', '{}{}'.format(
                message,
                ' ({})'.format(self.expect_failure_comment)
                if self.expect_failure_comment else ''
            ))
        else:
            self.result.set_status('FAILED', message)

    def set_passed(self):
        if self.expect_failure:
            msg = (
                'Failure was expected: {}'.format(self.expect_failure_comment)
                if self.expect_failure_comment else None
            )
            self.result.set_status('UOK', msg)
        else:
            self.result.set_status('PASSED')

    # Convenience path builders

    @property
    def testsuite_dir(self):
        """Return the absolute path to the testsuite root directory."""
        result = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              '..')
        return os.path.abspath(result)

    @property
    def test_dir(self):
        """Return the path of the current testcase directory."""
        return self.test_env['test_dir']

    def working_dir(self, *args):
        """
        Return the working dir, plus any path elements joined to it if passed
        in *args.
        """
        return os.path.join(self.global_env['working_dir'],
                            self.test_env['test_name'], *args)

    @property
    def output_file(self):
        return self.working_dir('actual.out')

    @property
    def expected_file(self):
        return self.working_dir('test.out')

    @property
    def original_expected_file(self):
        return os.path.join(self.test_dir, 'test.out')

    #
    # Tear up helpers
    #

    def check_file(self, filename):
        """
        Check file presence.

        If the file does not exist test is aborted.
        """
        if not os.path.isfile(os.path.join(self.test_dir, filename)):
            raise SetupError('Missing mandatory file: {}'.format(filename))

    def create_test_workspace(self):
        """
        Create a test workspace.

        This function copies the test sources into the working directory.
        """
        fileutils.sync_tree(self.test_dir, self.working_dir())

    #
    # Run helpers
    #

    def call(self, argv):
        """
        Run a subprocess with `argv`
        """
        Run(argv, cwd=self.working_dir(),
            timeout=self.TIMEOUT,
            output=self.output_file,
            error=STDOUT)
        self.result.actual_output += self.read_file(self.output_file)

    def call_and_check(self, argv):
        """
        Run a subprocess with `argv` and check it completes with status code 0.

        In case of failure, the status is appended to the actual output
        """

        p = Run(argv, cwd=self.working_dir(),
                timeout=self.TIMEOUT,
                output=self.output_file,
                error=STDOUT)
        self.result.actual_output += self.read_file(self.output_file)
        if p.status != 0:
            self.result.actual_output += \
                '>>>program returned status code %s\n' % p.status

    #
    # Analysis helpers
    #

    def analyze(self):

        def compared_lines(text):
            """
            Turn `text`, a multiline string, into a list of lines, LF
            characters not included and with the optional trailing CR character
            stripped.

            :type text: str
            :rtype: list[str]
            """
            # Don't use .rstrip so that only one CR is removed (if any)
            return [l[:-1] if l and l[-1] == '\r' else l
                    for l in text.split('\n')]

        def maybe_lower(text):
            """
            Depending on testsuite run settings, return `text` itself or
            lower-case it.
            """
            return (text if self.global_env['options'].strict_casing_diff else
                    text.lower())

        expected = maybe_lower(self.result.expected_output)
        actual = maybe_lower(self.result.actual_output)

        if self.global_env['options'].strict_whitespace_diff:
            # Rely directly on difflib rather than GNATpython's diff facilities
            # in order to test as much as possible all formatting aspects in
            # test outputs. Remember that testing covers a pretty-printer, so
            # performing strict comparisons is important.
            diff = list(difflib.unified_diff(
                a=compared_lines(expected), b=compared_lines(actual),
                fromfile='expected', tofile='output', lineterm=''))
            if diff:
                self.result.diff = '\n'.join(diff)
                self.result.set_status('FAILED', 'output diff')
            else:
                self.result.set_status('PASSED')
        else:
            # Do a case insensitive diff, because gnatpp does not yet generate
            # the correct case of identifiers. ???When that is fixed, change
            # this.
            self.analyze_diff(expected=expected, actual=actual)
