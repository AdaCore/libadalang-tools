
-----------------------------------------------------------------------------

with CXE4004_Common;  use CXE4004_Common;
package CXE4004_Part_A2 is
  -- This package supports the remote access tests
  pragma Remote_Call_Interface;

  procedure Call_With_2 (T : Integer_Vector);
  procedure Call_With_3 (T : Integer_Vector);

  type Remote_Proc is access procedure (X : Integer_Vector);

end CXE4004_Part_A2;
