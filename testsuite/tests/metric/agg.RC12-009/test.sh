echo test number 1
cd 1
gnatmetric -q -P agg.gpr
cat a1.ads.metrix
cat metrix.xml
cd ..

echo test number 2
cd 2
gnatmetric -q -XT=a_val -P agg.gpr
cat a.ads.metrix
cat metrix.xml
cd ..

echo test number 3
cd 3
rm -f c.obj d.obj
gnatmetric -q -P agg1.gpr
echo "----------------"
echo c.obj/c.ads.metrix
cat  c.obj/c.ads.metrix
echo "----------------"
echo c.obj/cc.ads.metrix
cat  c.obj/cc.ads.metrix
echo "----------------"
echo c.obj/metrix.xml
cat  c.obj/metrix.xml
echo "----------------"
echo d.obj/d.ads.metrix
cat  d.obj/d.ads.metrix
echo "----------------"
echo d.obj/dd.ads.metrix
cat  d.obj/dd.ads.metrix
echo "----------------"
echo d.obj/metrix.xml
cat  d.obj/metrix.xml
cd ..
