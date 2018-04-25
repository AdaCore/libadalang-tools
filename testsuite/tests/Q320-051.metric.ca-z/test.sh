gnatmetric --no-xml-config -q --all -x -sfn src/*.ad? --output-dir=actual
diff metrix.xml.expected metrix.xml | head
diff expected actual | head
