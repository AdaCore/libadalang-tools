#!/bin/sh

# Move parameter backward test cases

# Move parameter backward test case 0
move_parameter -P default.gpr -S main_package.ads -L 4 -R 36 -D backward

# Move parameter forward test cases

# Move parameter forward test case 0
move_parameter -P default.gpr -S main_package.ads -L 4 -R 19 -D forward
