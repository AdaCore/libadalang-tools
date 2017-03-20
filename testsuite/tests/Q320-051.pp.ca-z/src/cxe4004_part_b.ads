
-----------------------------------------------------------------------------

with CXE4004_Common;  use CXE4004_Common;
package CXE4004_Part_B is
  -- This package supports the remote access tests
  pragma Remote_Call_Interface;

  -- access test support
  procedure Call_With_4 (T : Integer_Vector);

end CXE4004_Part_B;
