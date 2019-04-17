from gnatpython.fileutils import rm

from testsuite_support.utils import run, show_nonprintable


rm('last-line-missing-crlf.ads.pp')
run('gnatpp --quiet --output=last-line-missing-crlf.ads.pp'
    ' last-line-missing-crlf.ads')
with open('last-line-missing-crlf.ads.pp', 'rb') as f:
    print(show_nonprintable(f.read()))
