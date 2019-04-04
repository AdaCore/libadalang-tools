gnatmetric --metrics-all --generate-xml-output --no-xml-config --short-file-names fors.adb
cat metrix.xml
cat fors.adb.metrix

gnatmetric --metrics-all --generate-xml-output --no-xml-config --short-file-names --no-static-loop fors.adb
cat metrix.xml
cat fors.adb.metrix
