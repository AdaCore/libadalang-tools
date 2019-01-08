gnatpp loop_then.adb --pipe --max-line-length=40

echo separate-loop
gnatpp loop_then.adb --pipe --max-line-length=40 --separate-loop
echo separate-then
gnatpp loop_then.adb --pipe --max-line-length=40 --separate-then
echo separate-loop-then
gnatpp loop_then.adb --pipe --max-line-length=40 --separate-loop-then
echo separate-loop separate-then
gnatpp loop_then.adb --pipe --max-line-length=40 --separate-loop --separate-then

echo no-separate-loop
gnatpp loop_then.adb --pipe --max-line-length=40 --no-separate-loop
echo no-separate-then
gnatpp loop_then.adb --pipe --max-line-length=40 --no-separate-then
echo no-separate-loop-then
gnatpp loop_then.adb --pipe --max-line-length=40 --no-separate-loop-then
echo no-separate-loop no-separate-then
gnatpp loop_then.adb --pipe --max-line-length=40 --no-separate-loop --no-separate-then
