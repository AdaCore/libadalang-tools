from e3.fs import rm

from drivers.utils import run, print_nonprintable


rm('last-line-missing-crlf.ads.pp')
run('gnatpp --quiet --output=last-line-missing-crlf.ads.pp'
    ' last-line-missing-crlf.ads')
print_nonprintable('last-line-missing-crlf.ads.pp')
