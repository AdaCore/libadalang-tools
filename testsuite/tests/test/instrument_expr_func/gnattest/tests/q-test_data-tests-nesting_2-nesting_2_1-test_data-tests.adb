--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Q.Test_Data.Tests.Nesting_2.Nesting_2_1.Test_Data.

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
package body Q.Test_Data.Tests.Nesting_2.Nesting_2_1.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_F5 (Gnattest_T : in out Test);
   procedure Test_F5_354a5c (Gnattest_T : in out Test) renames Test_F5;
--  id:2.2/354a5c8e2ee02230/F5/1/0/
   procedure Test_F5 (Gnattest_T : in out Test) is
   --  q.ads:10:10:F5
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Q.Nesting_2.Nesting_2_1.F5 (111) = 777,
         "wrong F5");

--  begin read only
   end Test_F5;
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
end Q.Test_Data.Tests.Nesting_2.Nesting_2_1.Test_Data.Tests;
