#!/bin/sh
suppress_separate -P default.gpr -S test.adb -L 2 -R 14
suppress_separate -P default.gpr -S test.adb -L 3 -R 14
suppress_separate -P default.gpr -S test-bar.adb -L 10 -R 27
suppress_separate -P default.gpr -S test-baz.adb -L 5 -R 10
