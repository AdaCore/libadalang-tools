rm -rf actual

echo "test_OK"
gnatpp --pipe test_OK.adb
echo $?

echo "test_NOK"
gnatpp --pipe test_NOK.adb
echo $?
