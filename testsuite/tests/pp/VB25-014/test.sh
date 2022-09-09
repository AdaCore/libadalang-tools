echo "Default layout"
gnatpp --pipe --layout=default main.adb
echo "Minimal layout"
gnatpp --pipe --layout=minimal main.adb
echo "Compact layout"
gnatpp --pipe --layout=compact main.adb
echo "Tall layout"
gnatpp --pipe --layout=tall main.adb
echo "Shorthand Tall layout with space syntax"
gnatpp --pipe -L tall main.adb
