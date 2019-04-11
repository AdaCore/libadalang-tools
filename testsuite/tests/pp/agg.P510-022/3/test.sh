gnatpp -q -P agg1.gpr
echo "----------------"
echo src_c/c.ads
cat  src_c/c.ads
echo "----------------"
echo src_c/cc.ads
cat  src_c/cc.ads
echo "----------------"
echo src_d/d.ads
cat  src_d/d.ads
echo "----------------"
echo src_d/dd.ads
cat  src_d/dd.ads
gnatmake -q -P agg1.gpr
