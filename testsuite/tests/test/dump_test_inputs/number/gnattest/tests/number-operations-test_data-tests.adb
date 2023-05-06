--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Number.Operations.Test_Data.

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
package body Number.Operations.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add (Gnattest_T : in out Test);
   procedure Test_Add_5658b0 (Gnattest_T : in out Test) renames Test_Add;
--  id:2.2/5658b0f836a1d0ef/Add/1/0/
   procedure Test_Add (Gnattest_T : in out Test) is
   --  number-operations.ads:7:4:Add
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Add (1, 1) = 2, "wrong addition");

--  begin read only
   end Test_Add;
--  end read only


--  begin read only
   procedure Test_Multiply (Gnattest_T : in out Test);
   procedure Test_Multiply_4339e5 (Gnattest_T : in out Test) renames Test_Multiply;
--  id:2.2/4339e5cefa15c38f/Multiply/1/0/
   procedure Test_Multiply (Gnattest_T : in out Test) is
   --  number-operations.ads:9:4:Multiply
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Multiply (3, 11) = 33, "wrong multiplication 1");
      AUnit.Assertions.Assert
        (Multiply (2, 4) = 8, "wrong multiplication 2");
      AUnit.Assertions.Assert
        (Multiply (1, 100) = 100, "wrong multiplication 2");

--  begin read only
   end Test_Multiply;
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
end Number.Operations.Test_Data.Tests;
