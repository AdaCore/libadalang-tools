gnatpp pp-formatting.adb --output=pp-formatting.adb.spaces-only --spaces-only

# -w ignores whitespace. The output should be identical,
# ignoring whitespace. Newlines don't count as whitespace.
diff -w pp-formatting.adb pp-formatting.adb.spaces-only
