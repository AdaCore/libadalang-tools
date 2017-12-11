gnatmetric -q --all -x -sfn src/*.ad? --output-dir=actual
diff metrix.xml.expected metrix.xml
diff expected actual
