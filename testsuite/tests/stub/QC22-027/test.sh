echo ================================================================
rm -f *.ads *.adb
gnatchop -q -w source.ada
echo gnatstub --update-body=4 pack.ads
gnatstub --update-body=4 pack.ads
gnatstub --quiet --update-body=4 pack.ads pack-child.ads
cat *.adb

echo ================================================================
rm -f *.ads *.adb
gnatchop -q -w source.ada
echo gnatstub --update-body=4 pack.ads --subunits
gnatstub --update-body=4 pack.ads --subunits
cat *.adb

echo ================================================================
echo pack-child.ads
gnatstub --update-body=9 pack-child.ads
gnatstub --update-body=7 pack-child.ads
gnatstub --update-body=17 pack-child.ads
gnatstub --update-body=18 pack-child.ads
cat *.adb

echo ================================================================
rm -f *.ads *.adb
gnatchop -q -w source.ada
echo alphabetical-order
gnatstub --update-body=7 pack-child.ads --alphabetical-order
gnatstub --update-body=17 pack-child.ads --alphabetical-order
gnatstub --update-body=18 pack-child.ads --alphabetical-order
cat *.adb

echo ================================================================
rm -f *.ads *.adb
gnatchop -q -w source.ada
echo subunits
gnatstub --update-body=7 pack-child.ads --subunits
gnatstub --update-body=17 pack-child.ads --subunits
gnatstub --update-body=18 pack-child.ads --subunits
cat *.adb

echo ================================================================
rm -f *.ads *.adb
gnatchop -q -w source.ada
echo alphabetical-order, subunits
gnatstub --update-body=7 pack-child.ads --subunits --alphabetical-order
gnatstub --update-body=17 pack-child.ads --subunits --alphabetical-order
gnatstub --update-body=18 pack-child.ads --subunits --alphabetical-order
cat *.adb

echo ================================================================
rm -f *.ads *.adb
gnatchop -q -w source.ada
echo mumble-processing.ads
gnatstub --update-body=18 mumble-processing.ads --subunits
gnatstub --update-body=19 mumble-processing.ads --subunits
gnatstub --update-body=26 mumble-processing.ads --subunits
gnatstub --update-body=27 mumble-processing.ads --subunits
gnatstub --update-body=36 mumble-processing.ads --subunits
cat *.adb
