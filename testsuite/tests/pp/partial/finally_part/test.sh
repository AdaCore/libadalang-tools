#! /usr/bin/env bash

# Whole subprogram body: the Handled_Stmts is formatted through
# Handled_Stmts_With_Begin_Alt_Partial_Mode, which must consume the optional
# F_Finally_Part subtree.
partial_gnatpp -P default.gpr -S main.adb -SL 1 -SC 1 -EL 6 -EC 10
# Just the statement list including the "finally" part.
partial_gnatpp -P default.gpr -S main.adb -SL 3 -SC 4 -EL 5 -EC 11
