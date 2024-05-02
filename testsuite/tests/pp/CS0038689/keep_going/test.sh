gnatpp --pipe --keep-going --quiet -P test.gpr 2>err_out
exitCode=$?
if [ $exitCode -eq 1 ]; then
    echo "Exit code was 1 as expected"
fi
grep Error err_out
