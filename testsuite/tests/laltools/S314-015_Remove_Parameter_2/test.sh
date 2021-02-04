#!/bin/sh
remove_parameter -P default.gpr -S main_package.ads -L 4 -R 19
remove_parameter -P default.gpr -S main_package.ads -L 6 -R 19
remove_parameter -P default.gpr -S main_package.ads -L 10 -R 19
remove_parameter -P default.gpr -S main_package.ads -L 15 -R 20
remove_parameter -P default.gpr -S main_package.ads -L 17 -R 21
remove_parameter -P default.gpr -S main.adb -L 17 -R 20