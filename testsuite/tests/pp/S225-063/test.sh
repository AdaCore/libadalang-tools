rm -rf last-line-missing-crlf.ads.pp
gnatpp --quiet --output=last-line-missing-crlf.ads.pp last-line-missing-crlf.ads
cat -v last-line-missing-crlf.ads.pp
