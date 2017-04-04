cd src
gnatmetric -v --all -x -sfn *.ad? > stdout
cat metrix.xml stdout *.metrix > ../actual
cd ..
diff expected actual | head
