# without .gpr file usage
partial_gnatpp -P default.gpr -S main.adb -SL 4 -SC 3 -EL 4 -EC 48
partial_gnatpp -P default.gpr -S main.adb -SL 4 -SC 3 -EL 4 -EC 48 --source-line-breaks

# with .gpr to take into account --call-threshold switch usage
partial_gnatpp -P default_pp.gpr -S main.adb -SL 4 -SC 3 -EL 4 -EC 48


