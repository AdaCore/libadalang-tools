#!/bin/sh

# Move parameter to the left test cases

# Move parameter to the left test case 0
move_parameter -P default.gpr -S main_package.ads -L 2 -R 19 -D left
# Move parameter to the left test case 1
move_parameter -P default.gpr -S main_package.ads -L 2 -R 35 -D left
# Move parameter to the left test case 2
move_parameter -P default.gpr -S main.adb -L 5 -R 38 -D left
# Move parameter to the left test case 3
move_parameter -P default.gpr -S main_package.adb -L 2 -R 51 -D left
# Move parameter to the left test case 4
move_parameter -P default.gpr -S main_package.adb -L 2 -R 54 -D left
# Move parameter to the left test case 5
move_parameter -P default.gpr -S main.adb -L 5 -R 22 -D left

# Move parameter to the right test cases

# Move parameter to the right test case 0
move_parameter -P default.gpr -S main_package.ads -L 3 -R 36 -D right
# Move parameter to the right test case 1
move_parameter -P default.gpr -S main_package.ads -L 2 -R 19 -D right
# Move parameter to the right test case 2
move_parameter -P default.gpr -S main_package.adb -L 2 -R 35 -D right
# Move parameter to the right test case 3
move_parameter -P default.gpr -S main_package.adb -L 2 -R 51 -D right
# Move parameter to the right test case 4
move_parameter -P default.gpr -S main.adb -L 5 -R 38 -D right
# Move parameter to the right test case 5
move_parameter -P default.gpr -S main.adb -L 5 -R 22 -D right
