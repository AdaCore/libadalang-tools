import difflib
import os
import os.path

from gnatpython import fileutils
from gnatpython.env import Env
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
        self.skip_test = False
        self.expect_failure = False

        status, self.message = self.run_control()
        if status == 'XFAIL':
            self.expect_failure = True
        elif status == 'SKIP':
            self.skip_test = True
        else:
            assert status is None and self.message is None

    @property
    def control_vars(self):
        """
        Return a dictionary to contain all values available in the control
        expressions (see the `run_control` method).

        :rtype: dict[str, object]
        """
        env = Env()
        return {
            'env': env,

            # Shortcuts to test the build OS
            'darwin': env.build.os.name == 'darwin',
            'linux': env.build.os.name == 'linux',
            'windows': env.build.os.name == 'windows',
        }

    def run_control(self):
        """
        If test.yaml has a "control" entry, run through its entries and either
        skip this test or put it on XFAIL depending on its content.

        The expected format for this entry is the following::

            control:
                - [XFAIL,
                   'linux or windows',
                   'Test is failing for now, will be fixed once XXX']
                - [SKIP, 'darwin', 'message', 'Test makes no sense on Darwin']

        This entry must contain an array of triples. Each triple contains:

        1. Either the XFAIL string or SKIP.
        2. A string that can be evaluated as a Python expression.
        3. A string that contains information about the control entry.

        The semantics of this entry is that the status of this test is the
        first entry for the first couple for which the expression evaluates to
        true. XFAIL means that the test is executed (result is XFAIL or UOK
        depending on its outcome), while SKIP means that the test is not
        executed (result is DEAD).

        Return the status that matched (if any) with the corresponding message,
        or (None, None) if no item matched.

        :rtype: (str, str) | (None, None)
        """
        # Evaluate variables available for expressions only once
        vars = self.control_vars

        # If there is a control entry, validate it entirely (don't stop at the
        # first entry) to detect errors in this specification as much as
        # possible, i.e. don't mask errors on the second item just because the
        # first one matched.
        items = []

        try:
            control = self.test_env['control']
        except KeyError:
            return (None, None)
        if not isinstance(control, list):
            raise SetupError('"control" must be an array')

        for i, item in enumerate(control):
            if not isinstance(item, list):
                raise SetupError('#{} entry in "control" must be an array'
                                 .format(i))
            if len(item) != 3:
                raise SetupError(
                    '#{} entry in "control" must be a 3-elements array'
                    .format(i))

            status, expr, message = item

            # Decode the status
            if status not in ('XFAIL', 'SKIP'):
                raise SetupError(
                    '#{} entry in "control" has invalid command: {}'
                    .format(i, status))

            # Execute the expression
            try:
                enabled = eval(expr, vars)
            except Exception as exc:
                raise SetupError(
                    '#{} entry in "control" has invalid expression:'
                    '{}: {}'
                    .format(i, type(exc).__name__, str(exc)))

            # Decode the message.  Because of wrapping in the YAML file, we can
            # get multi-line strings, which is not valid for comments.
            if not isinstance(message, basestring):
                raise SetupError(
                    '#{} entry in "control" has an invalid message'
                    .format(i))
            message = message.replace('\n', ' ').strip()

            items.append((status, enabled, message))

        for status, enabled, message in items:
            if enabled:
                return (status, message)
        return (None, None)

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
                ' ({})'.format(self.message)
                if self.message else ''
            ))
        else:
            self.result.set_status('FAILED', message)

    def set_passed(self):
        if self.expect_failure:
            msg = ('Failure was expected: {}'.format(self.message)
                   if self.message else None)
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

    def call(self, argv, env=None):
        """
        Run a subprocess with the given arguments and environment variables.

        This returns the Run() instance for the subprocess and appends the
        process output (stdout and stderr) to the result's actual_output field.

        :param list[str] argv: List of arguments for the subprocess to run.
        :param None|dict[str, str] env: None to inherit the environment
            variables, or a dictionary to override the environment passed to
            the subprocess.
        :rtype: Run
        """
        p = Run(argv, cwd=self.working_dir(), env=env,
                timeout=self.TIMEOUT,
                output=self.output_file,
                error=STDOUT)
        self.result.actual_output += self.read_file(self.output_file)
        return p

    def call_and_check(self, argv, env=None):
        """
        Run a subprocess with `argv` and check it completes with status code 0.

        In case of failure, the status is appended to the actual output.
        Arguments are the same as for the `call` method.
        """
        p = self.call(argv, env)
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
            self.result.diff = '\n'.join(diff)
        else:
            # Do a case insensitive diff, because gnatpp does not yet generate
            # the correct case of identifiers. ???When that is fixed, change
            # this.
            self.analyze_diff(expected=expected, actual=actual)

            # TestDriver.analyze_diff does not take our XFAIL mechanism into
            # account, so patch the result...

        if self.result.diff:
            self.set_failure('output diff')
        else:
            self.set_passed()

        # The --show-error-output option displays "actual_output". What users
        # need to read in order to understand the test failure is the diff
        # between actual output and expected one.
        self.result.actual_output = self.result.diff
