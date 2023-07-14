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
        (Fibonacci (6) = 8, "Wrong Fibonacci (8)");

--  begin read only
   end Test_Fibonacci;
--  end read only


--  begin read only
   procedure Test_Factorial (Gnattest_T : in out Test);
   procedure Test_Factorial_359894 (Gnattest_T : in out Test) renames Test_Factorial;
--  id:2.2/35989458fd499e2c/Factorial/1/0/
   procedure Test_Factorial (Gnattest_T : in out Test) is
   --  p.ads:6:4:Factorial
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

   declare
      Foo : Positive;
   begin
      Foo := Factorial (-1);
      AUnit.Assertions.Assert (False, "should have crashed");
   exception
      when Constraint_Error =>
         AUnit.Assertions.Assert (True, "properly crashed on negative input");
   end;
   
      AUnit.Assertions.Assert
        (Factorial (5) = 120, "Wrong Factorial (5)");

--  begin read only
   end Test_Factorial;
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
