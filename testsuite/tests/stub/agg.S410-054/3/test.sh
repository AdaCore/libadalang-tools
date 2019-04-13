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
gnatmake -q -P agg1.gpr
