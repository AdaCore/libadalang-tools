--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into P.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body P.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Fibonacci (Gnattest_T : in out Test);
   procedure Test_Fibonacci_539776 (Gnattest_T : in out Test) renames Test_Fibonacci;
--  id:2.2/539776bc9e80baa1/Fibonacci/1/0/
   procedure Test_Fibonacci (Gnattest_T : in out Test) is
   --  p.ads:3:4:Fibonacci
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Fibonacci_Rename (6) = 8, "Wrong Fibonacci expr func as body (8)");


--  begin read only
   end Test_Fibonacci;
--  end read only


--  begin read only
   procedure Test_Fibonacci_Rename (Gnattest_T : in out Test);
   procedure Test_Fibonacci_Rename_a8ab01 (Gnattest_T : in out Test) renames Test_Fibonacci_Rename;
--  id:2.2/a8ab015f3bed7224/Fibonacci_Rename/1/0/
   procedure Test_Fibonacci_Rename (Gnattest_T : in out Test) is
   --  p.ads:6:4:Fibonacci_Rename
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Fibonacci_Rename (6) = 8, "Wrong Fibonacci remaming as body (8)");

--  begin read only
   end Test_Fibonacci_Rename;
--  end read only


--  begin read only
   procedure Test_Stub_Body (Gnattest_T : in out Test);
   procedure Test_Stub_Body_5c46a1 (Gnattest_T : in out Test) renames Test_Stub_Body;
--  id:2.2/5c46a1e98fd20f3d/Stub_Body/1/0/
   procedure Test_Stub_Body (Gnattest_T : in out Test) is
   --  p.ads:9:4:Stub_Body
--  end read only

      pragma Unreferenced (Gnattest_T);

      X : Integer := 10;
      Y : Boolean := True;
   begin

      Stub_Body (X, Y);
      AUnit.Assertions.Assert (X = 10, "X modified");
      AUnit.Assertions.Assert (Y, "Y modified");

--  begin read only
   end Test_Stub_Body;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end P.Test_Data.Tests;
