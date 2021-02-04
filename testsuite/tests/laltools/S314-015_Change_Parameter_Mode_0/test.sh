#!/bin/sh

change_parameter_mode -P default.gpr -S main_package.ads -L 3 -R 7 -M in
change_parameter_mode -P default.gpr -S main_package.ads -L 4 -R 10 -M out
change_parameter_mode -P default.gpr -S main_package.ads -L 5 -R 13 -M 'in out'
change_parameter_mode -P default.gpr -S main_package.ads -L 6 -R 24 -M default
change_parameter_mode -P default.gpr -S main_package.ads -L 4 -R 20 -M out
change_parameter_mode -P default.gpr -S main_package.ads -L 9 -R 30 -M out