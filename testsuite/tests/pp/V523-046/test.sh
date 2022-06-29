rm -f foo_formated*
gnatpp foo.ads --comments-gnat-beginning --output foo_formatted.ads
gnatpp foo.adb --comments-gnat-beginning --output foo_formatted.adb
gnatpp foo.ads --alignment --attribute-mixed-case --based-grouping=4 --call_threshold=4 --comments-gnat-beginning --decimal-grouping=3 --enum-case-as-declared --indent-continuation=2 --indentation=3 --keyword-lower-case --max-line-length=99 --name-case-as-declared --no-compact --number-case-as-declared --par_threshold=0 --pragma-mixed-case --split-line-before-op --split-line-before-record --type-case-as-declared --output foo_formatted_2.ads
diff foo_formatted.ads foo_expected.ads
diff foo_formatted.adb foo_expected.adb
diff foo_formatted_2.ads foo_expected_2.ads

