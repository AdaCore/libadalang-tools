#!/usr/bin/env python

import os
import re

from e3.os.process import Run


def contents_of(filename):
    with open(filename) as f:
        return "\n".join(f.readlines())


def test(arg):
    actual_p = Run(["gnattest", "-Psimple", f"--dump-subp-hash={arg}"])
    if actual_p.status != 0:
        print(f"Error running {actual_p.command_line_image}:")
        print(actual_p.out)
        exit(1)
    actual_hash = actual_p.out.strip()

    expected_p = Run(["gnattest", "-Psimple", "simple.ads", "--gen-test-vectors"])
    if expected_p.status != 0:
        print(f"Error running {expected_p.command_line_image}:")
        print(expected_p.out)
        exit(1)

    baseline_filename = "obj/gnattest/tests/JSON_Tests/simple.json"
    hash_search = re.search(
        r"^..([a-z]|[0-9]){16}",
        contents_of(baseline_filename)
    )
    if not hash_search:
        print(f"Could not find baseline hash in {baseline_filename}")
        exit(1)

    expected_hash = hash_search.group(0)[2:]
    if expected_hash != actual_hash:
        print(f"error: The two hashes do not match. expected: {expected_hash}")
        print(f"but got: {actual_hash}")
        exit(1)

    print("success")


os.chdir("test")

test("src/simple.ads:5")
test("simple.ads:5")

file = os.path.abspath("src/simple.ads")
line = ":5"
test(file+line)
