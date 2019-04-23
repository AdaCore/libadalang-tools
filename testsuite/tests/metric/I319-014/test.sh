echo ignore-assertions
gnatmetric assertions.ads assertions.adb -q -x -nt -sfn --ignore-assertions
cat metrix.xml
echo no-ignore-assertions
gnatmetric assertions.ads assertions.adb -q -x -nt -sfn --no-ignore-assertions
cat metrix.xml
