rm -f *.ads *.adb
gnatchop -q -w source.ada
gnatstub --quiet --subunits p.ads
gnatmake -q -f -g -O0 -gnata p-main.adb
p-main
