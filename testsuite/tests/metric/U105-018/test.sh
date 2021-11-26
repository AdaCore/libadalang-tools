rm -rf obj

gnatmetric -ox metrix.xml -P simple.gpr --short-file-names -Xmode=gmfiles simple.adb
cat obj/gmfiles/simple.adb.metrix
