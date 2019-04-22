echo static
gnatmetric --metrics-all --generate-xml-output --no-xml-config --short-file-names static.adb
cat metrix.xml
cat static.adb.metrix

echo static no-static-loop
gnatmetric --metrics-all --generate-xml-output --no-xml-config --short-file-names --no-static-loop static.adb
cat metrix.xml
cat static.adb.metrix

echo nonstatic
gnatmetric --metrics-all --generate-xml-output --no-xml-config --short-file-names nonstatic.adb
cat metrix.xml
cat nonstatic.adb.metrix

echo nonstatic no-static-loop
gnatmetric --metrics-all --generate-xml-output --no-xml-config --short-file-names --no-static-loop nonstatic.adb
cat metrix.xml
cat nonstatic.adb.metrix
