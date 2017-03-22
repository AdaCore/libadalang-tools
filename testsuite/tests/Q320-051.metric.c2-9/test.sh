cd src
gnatmetric -v --all -x -sfn *.ad? > stdout
cat metrix.xml stdout *.metrix > actual
diff expected actual | head
