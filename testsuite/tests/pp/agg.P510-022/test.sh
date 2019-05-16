echo test number 1
cd 1
gnatpp -pipe -q -P agg.gpr
cd ..

echo test number 2
cd 2
gnatpp -q --pipe -XT=a_val -P agg.gpr
cd ..

echo test number 3
cd 3
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
cd ..
