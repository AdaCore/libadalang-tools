gnattest -P p -q
gprbuild -q -P h/test_driver.gpr
echo "simple case"
h/test_runner --routines=simple.ads:3
echo "generics"
h/test_runner --routines=sorting_algorithms.ads:9
echo "oop (inheritance)"
h/test_runner --routines=speed1.ads:6
echo "oop (substitution)"
h/test_runner --routines=speed1.ads:7
echo "contracts"
h/test_runner --routines=contracts.ads:5
echo "multiple"
h/test_runner --routines=simple.ads:5 --routines=speed2.ads:10
echo "from file"
h/test_runner --routines=@list.txt
echo "bad sloc from cmd"
h/test_runner --routines=bad_sloc.ads:123
echo "bad sloc from file"
h/test_runner --routines=@bad_list.txt
rm -rf h
gnattest -P p -q --no-test-filtering-file-io
gprbuild -q -P h/test_driver.gpr
echo "file support off"
h/test_runner --routines=@list.txt
rm -rf h
gnattest -P p -q --no-test-filtering
gprbuild -q -P h/test_driver.gpr
