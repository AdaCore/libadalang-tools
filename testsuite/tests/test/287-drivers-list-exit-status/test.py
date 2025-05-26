#!/usr/bin/env python

import os
import re

from e3.os.process import Run

def change_dir(path, msg):
    os.chdir(path)
    print("")
    print(msg)
    print ("")

def check(actual, expected):
    if actual != expected:
        print("KO: Expected "
              + str(expected)
              + ", got "
              + str(actual)
              + " exit status.")
        success = False
    else:
        print ("OK")

def run_test(expected_exit_status, exit_val, passed_tests_val, msg):
    print (msg)

    add_args = ["--exit-status=" + exit_val,
                "--passed-tests=" + passed_tests_val]

    gen_args = ["gnattest",
                "-Ptest.gpr", "--stub", "--tests-dir", "../tests", "-q"] + add_args
    gpr_args = ["gprbuild", "-q", "-P", "obj/gnattest_stub/harness/test_drivers.gpr"]
    run_args = ["gnattest",
                "obj/gnattest_stub/harness/test_drivers.list"] + add_args

    Run(gen_args)
    Run(gpr_args)
    res = Run(run_args)
    check (res.status, expected_exit_status)

# PASS
change_dir("pass","# All tests passing #")

run_test(0, "on",  "show", "# Enabling --exit-status")
run_test(0, "on",  "hide", "# Enabling --exit-status, hiding passed tests")
run_test(0, "off", "show", "# Disabling --exit-status")

# FAIL
change_dir("../fail", "# All tests failing #")

run_test(1, "on",  "show", "# Enabling --exit-status")
run_test(0, "off", "show", "# Disabling --exit-status")

# MIX
change_dir("../mix", "# One test failing, three passing #")

run_test(1, "on",  "show", "# Enabling --exit-status")
run_test(1, "on",  "hide", "# Enabling --exit-status, hiding passed tests")
run_test(0, "off", "show", "# Disabling --exit-status")
