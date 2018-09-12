rm -f *.ads *.adb
gnatchop -q -w source.ada
gnatstub --update-body=4 pack.ads
cat pack.adb

rm -f *.ads *.adb
gnatchop -q -w source.ada
gnatstub --update-body=4 pack.ads --subunits
cat pack.adb pack-inner.adb pack-proc.adb
