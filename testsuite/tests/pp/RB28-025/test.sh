rm -rf out *.npp abc.ads
cp abc.ada abc.ads

echo gnatpp --pipe abc.ads
gnatpp --pipe abc.ads

echo gnatpp --replace_backup abc.ads
gnatpp --replace_backup abc.ads
cat abc.ads.npp abc.ads
echo gnatpp --replace_backup abc.ads
gnatpp --replace_backup abc.ads
cp abc.ada abc.ads
echo gnatpp --replace_force_backup abc.ads
gnatpp --replace_force_backup abc.ads
cat abc.ads.npp abc.ads

cp abc.ada abc.ads
echo gnatpp --replace_backup --output-dir=out abc.ads
gnatpp --replace_backup --output-dir=out abc.ads
cat out/abc.ads.npp abc.ads
echo gnatpp --replace_backup --output-dir=out abc.ads
gnatpp --replace_backup --output-dir=out abc.ads
cp abc.ada abc.ads
echo gnatpp --replace_force_backup --output-dir=out abc.ads
gnatpp --replace_force_backup --output-dir=out abc.ads
cat out/abc.ads.npp abc.ads
