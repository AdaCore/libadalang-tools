gnatmetric -q --all --generate-xml-output --short-file-names -P proj.gpr --output-dir=actual
diff metrix.xml.expected metrix.xml | head -400
diff expected actual | head -400
