#!/bin/sh
outgoing_calls -P p.gpr -S ./src/main.adb -L 2 -R 11
outgoing_calls -P p.gpr -S ./src/main.adb -L 4 -R 15
outgoing_calls -P p.gpr -S ./src/main.adb -L 4 -R 4
outgoing_calls -P p.gpr -S ./src/main.adb -L 27 -R 4
