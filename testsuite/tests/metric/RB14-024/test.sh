gnatmetric --quiet --short_file_names --xml_file_name=obj/metrix.xml -x -Pp.gpr
cat obj/metrix.xml obj/p.ads.metrix obj/hello.adb.metrix
