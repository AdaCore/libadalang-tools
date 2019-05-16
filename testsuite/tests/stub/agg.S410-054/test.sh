echo test number 1
cd 1
gnatstub -q -P agg.gpr
cat src_a1/a1.adb
cd ..

echo test number 2
cd 2
gnatstub -q -XT=a_val -P agg.gpr
cat src_a/a.adb
cd ..

echo test number 3
cd 3
rm -f c.obj d.obj
gnatstub -q -P agg1.gpr
echo "----------------"
echo src_c/c.adb
cat  src_c/c.adb
echo "----------------"
echo src_c/cc.adb
cat  src_c/cc.adb
echo "----------------"
echo src_d/d.adb
cat  src_d/d.adb
echo "----------------"
echo src_d/dd.adb
cat  src_d/dd.adb
gnatmake -q -P agg1.gpr 2>&1 | sort
cd ..
