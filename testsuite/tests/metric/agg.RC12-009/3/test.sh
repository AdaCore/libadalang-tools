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
