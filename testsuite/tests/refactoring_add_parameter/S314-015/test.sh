#!/bin/sh

# The input to the test driver is the cursor location (source file, line
# number and column number) and the new parameter to be added (which
# depending on the cursor location, can either be a full parameter
# specification, an identifier, or an identifier list).

# Set 1

# This first set of test cases add fully defined parameters

# Add a parameter to a subprogram without parameters
# Initial Spec:
# procedure Foo;
# Final Spec:
# procedure Foo (A, B : Integer);
add_parameter -P default.gpr -S main_package.ads -L 3 -R  4 \
    -N 'A, B : Integer'
add_parameter -P default.gpr -S main_package.ads -L 3 -R 13 \
    -N 'A, B : Integer'
add_parameter -P default.gpr -S main_package.ads -L 3 -R 17 \
    -N 'A, B : Integer'
## Invalid input
add_parameter -P default.gpr -S main_package.ads -L 3 -R  3 \
    -N 'A, B : Integer'
add_parameter -P default.gpr -S main_package.ads -L 3 -R 18 \
    -N 'A, B : Integer'
add_parameter -P default.gpr -S main_package.ads -L 3 -R  4
add_parameter -P default.gpr -S main_package.ads -L 3 -R  4 \
    -N 'A'

# Add a parameter as the first parameter of a subprogram that has at least one
# parameter
# Initial Subp_Spec
# procedure Bar (A : Integer)
# Final Subp_Spec
# procedure Bar (B : String; A : Integer)
add_parameter -P default.gpr -S main_package.ads -L 5 -R  4 \
    -N 'B : String'
add_parameter -P default.gpr -S main_package.ads -L 5 -R 18 \
    -N 'B : String'
## Invalid input
add_parameter -P default.gpr -S main_package.ads -L 5 -R  4 \
    -N 'B^ : String'

# Add a parameter as the second parameter of a subprogram that only has one
# parameter
# Initial Subp_Spec
# procedure Bar (A : Integer)
# Final Subp_Spec
# procedure Bar (A : Integer; B : String);
add_parameter -P default.gpr -S main_package.ads -L 5 -R 19 \
    -N 'B : String'
add_parameter -P default.gpr -S main_package.ads -L 5 -R 31 \
    -N 'B : String'

# Add a parameter as not the first not the last parameter of a subprogram that
# has at least two parameters
# Initial Subp_Spec
# procedure Baz (A : in Integer; B : out Float; C : in out String);
# Final Subp_Spec
# procedure Baz (A : in Integer; D : String; B : out Float; C : in out String);
add_parameter -P default.gpr -S main_package.ads -L 7 -R 19 \
    -N 'D : String'
add_parameter -P default.gpr -S main_package.ads -L 7 -R 33 \
    -N 'D : String'
# Initial Subp_Spec
# procedure Baz (A : in Integer; B : out Float; C : in out String);
# Final Subp_Spec
# procedure Baz (A : in Integer; B : out Float; D : String; C : in out String);
add_parameter -P default.gpr -S main_package.ads -L 7 -R 34 \
    -N 'D : String'
add_parameter -P default.gpr -S main_package.ads -L 7 -R 35 \
    -N 'D : String'
add_parameter -P default.gpr -S main_package.ads -L 7 -R 48 \
    -N 'D : String'

# Add a parameter as the last parameter of a subprogram that has at least two
# parameters
# Initial Subp_Spec
# procedure Baz (A : in Integer; B : out Float; C : in out String);
# Final Subp_Spec
# procedure Baz (A : in Integer; D : String; B : out Float; C : in out String);
add_parameter -P default.gpr -S main_package.ads -L 7 -R 49 \
    -N 'D : String'
add_parameter -P default.gpr -S main_package.ads -L 7 -R 50 \
    -N 'D : String'
add_parameter -P default.gpr -S main_package.ads -L 7 -R 68 \
    -N 'D : String'

# Add a parameter before a list of parameters, on a subp that has multiple param
# specs
# Initial Subp_Spec
# procedure Waldo (A, B : Integer; C, D : out Float);
# Final Subp_Spec
# procedure Waldo (E : String; A, B : Integer; C, D : out Float);
add_parameter -P default.gpr -S main_package.ads -L 9 -R 20 \
    -N 'E : String'

# Add a parameter between a list of parameters of a param spec, on a subp that
# has multiple param specs
# Initial Subp_Spec
# procedure Waldo (A, B : Integer; C, D : out Float);
# Final Subp_Spec
# procedure Waldo (A : Integer; E : String; B : Integer; C, D : out Float);
add_parameter -P default.gpr -S main_package.ads -L 9 -R 21 \
    -N 'E : String'
add_parameter -P default.gpr -S main_package.ads -L 9 -R 23 \
    -N 'E : String'

# Add a parameter after a list of parameters, on a subp that has multiple param
# specs
# Initial Subp_Spec
# procedure Waldo (A, B : Integer; C, D : out Float);
# Final Subp_Spec
# procedure Waldo (A, B : Integer; E : String; C, D : out Float);
add_parameter -P default.gpr -S main_package.ads -L 9 -R 24 \
    -N 'E : String'
add_parameter -P default.gpr -S main_package.ads -L 9 -R 35 \
    -N 'E : String'

# Add a parameter between a list of parameters, on a subp that only has one
# param spec
# Initial Subp_Spec
# procedure Cargo (A, B, C : Integer);
# Final Subp_Spec
# procedure Cargo (A, B : Integer; D : String; C : Integer);
add_parameter -P default.gpr -S main_package.ads -L 11 -R 26 \
    -N 'D : String'

# Add a after between a list of parameters, on a subp that only has one param
# spec
# Initial Subp_Spec
# procedure Cargo (A, B, C : Integer);
# Final Subp_Spec
# procedure Cargo (A, B, C : Integer; D : String);
add_parameter -P default.gpr -S main_package.ads -L 11 -R 27 \
    -N 'D : String'
add_parameter -P default.gpr -S main_package.ads -L 11 -R 38 \
    -N 'D : String'
add_parameter -P default.gpr -S main_package.ads -L 11 -R 39 \
    -N 'D : String'

# Set 2

# This second set of test cases simply add identifiers

# Add a parameter as the first parameter of a subprogram that has one parameter
# Initial Subp_Spec
# procedure Bar (A : Integer)
# Final Subp_Spec
# procedure Bar (A, X : String)
add_parameter -P default.gpr -S main_package.adb -L 8 -R 7 \
    -N 'X'
add_parameter -P default.gpr -S main_package.adb -L 8 -R 18 \
    -N 'X'

# Add two parameters as the last parameters of a subprogram that has one
# parameter
# Initial Subp_Spec
# procedure Bar (A : Integer)
# Final Subp_Spec
# procedure Bar (A, X, Y : String)
add_parameter -P default.gpr -S main_package.adb -L 8 -R 19 \
    -N 'X, Y'
add_parameter -P default.gpr -S main_package.adb -L 8 -R 20 \
    -N 'X, Y'

# Add a parameter before and after an unique parameter of a param spec, on a
# subprogram with multiple param specs
# procedure Baz (A : in Integer; B : out Float; C : in out String)
# Final Subp_Spec
# procedure Baz (X, A : in Integer; B : out Float; C : in out String)
# procedure Baz (A, X : in Integer; B : out Float; C : in out String)
# procedure Baz (A, X : in Integer; B : out Float; C : in out String)
# procedure Baz (A : in Integer; X, B : out Float; C : in out String)
# procedure Baz (A : in Integer; B, X : out Float; C : in out String)
# procedure Baz (A : in Integer; B, X : out Float; C : in out String)
# procedure Baz (A : in Integer; B : out Float; X, C : in out String)
# procedure Baz (A : in Integer; B : out Float; C, X : in out String)
# procedure Baz (A : in Integer; B : out Float; C, X : in out String)
add_parameter -P default.gpr -S main_package.adb -L 13 -R 18 \
    -N 'X'
add_parameter -P default.gpr -S main_package.adb -L 13 -R 19 \
    -N 'X'
add_parameter -P default.gpr -S main_package.adb -L 13 -R 20 \
    -N 'X'
add_parameter -P default.gpr -S main_package.adb -L 13 -R 34 \
    -N 'X'
add_parameter -P default.gpr -S main_package.adb -L 13 -R 35 \
    -N 'X'
add_parameter -P default.gpr -S main_package.adb -L 13 -R 36 \
    -N 'X'
add_parameter -P default.gpr -S main_package.adb -L 13 -R 49 \
    -N 'X'
add_parameter -P default.gpr -S main_package.adb -L 13 -R 50 \
    -N 'X'
add_parameter -P default.gpr -S main_package.adb -L 13 -R 51 \
    -N 'X'

# Add a parameter before and after a parameter on a param spec with multiple
# parameters, on a subprogram with multiple param specs
# procedure Waldo (A, B : Integer; C, D : out Float)
# Final Subp_Spec
# procedure Waldo (X, A, B : Integer; C, D : out Float)
# procedure Waldo (A, X, B : Integer; C, D : out Float)
# procedure Waldo (A, X, B : Integer; C, D : out Float)
# procedure Waldo (A, B, X : Integer; C, D : out Float)
# procedure Waldo (A, B : Integer; X, C, D : out Float)
# procedure Waldo (A, B : Integer; C, X, D : out Float)
# procedure Waldo (A, B : Integer; C, D, X : out Float)
add_parameter -P default.gpr -S main_package.adb -L 18 -R 20 \
    -N 'X'
add_parameter -P default.gpr -S main_package.adb -L 18 -R 21 \
    -N 'X'
add_parameter -P default.gpr -S main_package.adb -L 18 -R 23 \
    -N 'X'
add_parameter -P default.gpr -S main_package.adb -L 18 -R 24 \
    -N 'X'
add_parameter -P default.gpr -S main_package.adb -L 18 -R 35 \
    -N 'X'
add_parameter -P default.gpr -S main_package.adb -L 18 -R 37 \
    -N 'X'
add_parameter -P default.gpr -S main_package.adb -L 18 -R 40 \
    -N 'X'

# Add a parameter before and after the first parameter of a subprogram
# with only one subp spec with multiple parameters
# procedure Cargo (A, B, C : Integer)
# Final Subp_Spec
# procedure Cargo (A, X, B, C : Integer)
add_parameter -P default.gpr -S main_package.adb -L 23 -R 21 \
    -N 'X'
