#!/bin/sh

# Move parameter to the left test cases

# Move parameter to the left test case 0
move_parameter -P default.gpr -S main_package.ads -L 4 -R 36 -D left

# Move parameter to the right test cases

# Move parameter to the right test case 0
move_parameter -P default.gpr -S main_package.ads -L 4 -R 19 -D right
