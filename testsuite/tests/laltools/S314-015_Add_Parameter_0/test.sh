#!/bin/sh
add_parameter -P default.gpr -S main_package.ads -L 3 -R 14
add_parameter -P default.gpr -S main_package.ads -L 3 -R 14 -N Bar -T Integer
add_parameter -P default.gpr -S main_package.ads -L 3 -R 14 -N Bar -M 'in out' -T Integer
add_parameter -P default.gpr -S main_package.ads -L 3 -R 14 -N Bar -T Integer -D 1
add_parameter -P default.gpr -S main_package.ads -L 3 -R 14 -N Bar -M 'in out' -T Integer -D 1
add_parameter -P default.gpr -S main_package.ads -L 5 -R 14
add_parameter -P default.gpr -S main_package.ads -L 5 -R 14 -N Baz -T Integer
add_parameter -P default.gpr -S main_package.ads -L 5 -R 14 -N Baz -M 'in out' -T Integer
add_parameter -P default.gpr -S main_package.ads -L 5 -R 14 -N Baz -T Integer -D 1
add_parameter -P default.gpr -S main_package.ads -L 5 -R 14 -N Baz -M 'in out' -T Integer -D 1
add_parameter -P default.gpr -S main_package.ads -L 5 -R 19
add_parameter -P default.gpr -S main_package.ads -L 5 -R 19 -N Baz -T Integer
add_parameter -P default.gpr -S main_package.ads -L 5 -R 19 -N Baz -M 'in out' -T Integer
add_parameter -P default.gpr -S main_package.ads -L 5 -R 19 -N Baz -T Integer -D 1
add_parameter -P default.gpr -S main_package.ads -L 5 -R 19 -N Baz -M 'in out' -T Integer -D 1
add_parameter -P default.gpr -S main_package.ads -L 7 -R 19
add_parameter -P default.gpr -S main_package.ads -L 7 -R 19 -N Baz -T Integer
add_parameter -P default.gpr -S main_package.ads -L 7 -R 19 -N Baz -M 'in out' -T Integer
add_parameter -P default.gpr -S main_package.ads -L 7 -R 19 -N Baz -T Integer -D 1
add_parameter -P default.gpr -S main_package.ads -L 7 -R 19 -N Baz -M 'in out' -T Integer -D 1