# Regression tests for file-name arguments that contain '[' but are not meant
# as wildcard patterns. See Utils.Command_Lines.

# 1. "weird[set].adb" names an existing file; as a glob it would only match
#    "weirds.adb"/"weirde.adb"/... so before the fix the argument expanded to
#    nothing and the file was silently skipped. gnatpp must now treat the
#    existing file literally and format it (Ada.Directories.Exists guard).
cp base.adb 'weird[set].adb'
gnatpp --pipe 'weird[set].adb'

# 2. A wildcard argument ('[' present) that ends in a '\' made GNAT.Command_Line
#    hand the pattern to System.Regexp, which raised
#      ERROR_IN_REGEXP : Incorrect character '\' in regular expression
#    (a '\' is only legal mid-pattern as an escape, never as the last char).
#    On Windows every path separator is a '\', so this crashed gnatpp whenever a
#    path component contained '[' and the path ended in a separator. It must now
#    be caught and the argument treated literally (then reported as a missing
#    file) rather than crashing with an unhandled exception.
gnatpp --pipe 'foo[x]\'
