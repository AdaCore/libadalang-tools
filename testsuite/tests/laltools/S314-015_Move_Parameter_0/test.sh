#!/bin/sh

# Move parameter backward test cases

# Move parameter backward test case 0
move_parameter -P default.gpr -S main_package.ads -L 2 -R 19 -D backward
# Move parameter backward test case 1
move_parameter -P default.gpr -S main_package.ads -L 2 -R 35 -D backward
# Move parameter backward test case 2
move_parameter -P default.gpr -S main.adb -L 5 -R 38 -D backward
# Move parameter backward test case 3
move_parameter -P default.gpr -S main_package.adb -L 2 -R 51 -D backward
# Move parameter backward test case 4
move_parameter -P default.gpr -S main_package.adb -L 2 -R 54 -D backward
# Move parameter backward test case 5
move_parameter -P default.gpr -S main.adb -L 5 -R 22 -D backward

# Move parameter forward test cases

# Move parameter forward test case 0
move_parameter -P default.gpr -S main_package.ads -L 3 -R 36 -D forward
# Move parameter forward test case 1
move_parameter -P default.gpr -S main_package.ads -L 2 -R 19 -D forward
# Move parameter forward test case 2
move_parameter -P default.gpr -S main_package.adb -L 2 -R 35 -D forward
# Move parameter forward test case 3
move_parameter -P default.gpr -S main_package.adb -L 2 -R 51 -D forward
# Move parameter forward test case 4
move_parameter -P default.gpr -S main.adb -L 5 -R 38 -D forward
# Move parameter forward test case 5
move_parameter -P default.gpr -S main.adb -L 5 -R 22 -D forward
