#! /usr/bin/env bash

# Single statement
partial_gnatpp -P default.gpr -S main.adb -SL 6 -SC 6 -EL 7 -EC 7
# Multiple statements with pre and post comments
partial_gnatpp -P default.gpr -S main.adb -SL 5 -SC 6 -EL 11 -EC 7



