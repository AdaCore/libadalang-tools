from __future__ import print_function

import shlex
import subprocess


def run(args):
    """
    Run a command, just like a shell script would, but also print non-zero
    status code.

    :type args: str
    """
    returncode = subprocess.call(shlex.split(args))
    if returncode:
        print('>>> non-zero return code for {}: {}'
              .format(args, returncode))


def show_nonprintable(text):
    """
    Transform `text` as if it was pipe'd through `cat -v`.

    :type text: str
    :rtype: str
    """
    def escape(char):
        s = chr(char)
        if char > 0x7f:
            return 'M-' + escape(chr(char - 0o200))

        if s in ('\n', '\t'):
            return s
        elif char < ord(' '):
            return '^' + chr(char + ord('@'))
        elif char == 0x7f:
            return '^?'
        else:
            return s

    return ''.join(escape(c) for c in text)


def print_nonprintable(filename):
    """
    Print the output of show_nonprintable on the given file.

    :param str filename: Name of the file to read.
    """
    with open(filename, 'rb') as f:
        print(show_nonprintable(f.read()), end='')
