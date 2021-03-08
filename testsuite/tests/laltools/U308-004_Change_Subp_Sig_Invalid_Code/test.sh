#!/bin/sh
add_parameter -P default.gpr -S main.adb -L 3 -R 16
change_parameter_mode -P default.gpr -S main.adb -L 3 -R 16 -M default
move_parameter -P default.gpr -S main.adb -L 3 -R 16 -D forward
remove_parameter -P default.gpr -S main.adb -L 3 -R 16
