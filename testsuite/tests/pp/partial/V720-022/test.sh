
# partial line selection
partial_gnatpp -P default.gpr -S main.adb -SL 5 -SC 7 -EL 5 -EC 17
partial_gnatpp -P default.gpr -S main.adb -SL 5 -SC 7 -EL 7

# cursor position
partial_gnatpp -P default.gpr -S main.adb -SL 5 -SC 5 -EL 5 -EC 5

# one line selection with filtering
partial_gnatpp -P default.gpr -S main.adb -SL 5 -SC 5 -EL 5 -EC 5 --source-line-breaks

# multiple lines selection with filtering
partial_gnatpp -P default.gpr -S main.adb -SL 5 -SC 5 -EL 9 --source-line-breaks

# whole file selection
partial_gnatpp -P default.gpr -S main.adb -SL 1 -EL 14


