rm -rf actual

gnatpp\
	 --call-threshold=1\
	 --par-threshold=1\
	 --vertical-enum-types\
	 --vertical-named-aggregates\
	 -W8\
	 -c5\
	 -c4\
	 --output-dir=actual test.ads

diff -r expected actual
