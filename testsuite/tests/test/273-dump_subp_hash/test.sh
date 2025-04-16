#!/bin/bash

test() {
gnattest -Psimple --dump-subp-hash=$1 1> my_hash 2>&1
gnattest -Psimple simple.ads --gen-test-vectors
grep -o -E "^..([a-z]|[0-9]){16}" obj/gnattest/tests/JSON_Tests/simple.json | cut -c 3- > hash_ref

if cmp -s my_hash hash_ref; then
    printf 'success\n'
else
    printf 'error: The two hashes do not match. expected:\n'
    cat hash_ref
    printf 'but got:\n'
    cat my_hash
    printf '\n'
fi;

rm my_hash
rm hash_ref
}

cd test/

test "src/simple.ads:5"
test "simple.ads:5"

file=$(realpath "src/simple.ads")
line=":5"
path="$file$line"

test $path
