echo "Testing --layout switch"

echo "1 --layout=default"
gnatpp -P default.gpr main.adb --layout=default --pipe
echo "---------------------------------------------------"
echo "2 --layout=minimal"
gnatpp -P default.gpr main.adb --layout=minimal --pipe
echo "---------------------------------------------------"
echo "3 --layout=tall"
gnatpp -P default.gpr main.adb --layout=tall --pipe
echo "---------------------------------------------------"
echo "4 --layout=compact"
gnatpp -P default.gpr main.adb --layout=compact --pipe

