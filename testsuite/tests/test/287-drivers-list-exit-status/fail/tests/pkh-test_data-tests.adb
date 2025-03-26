--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Pkh.Test_Data.

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
package body Pkh.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Foo2 (Gnattest_T : in out Test);
   procedure Test_Foo2_9e0267 (Gnattest_T : in out Test) renames Test_Foo2;
--  id:2.2/9e0267c272523a2f/Foo2/1/0/
   procedure Test_Foo2 (Gnattest_T : in out Test) is
   --  pkh.ads:2:5:Foo2
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert (False, "Always fails");

--  begin read only
   end Test_Foo2;
--  end read only


--  begin read only
   procedure Test_Bar2 (Gnattest_T : in out Test);
   procedure Test_Bar2_6e99fa (Gnattest_T : in out Test) renames Test_Bar2;
--  id:2.2/6e99fa557989b1e8/Bar2/1/0/
   procedure Test_Bar2 (Gnattest_T : in out Test) is
   --  pkh.ads:3:5:Bar2
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert (False, "Always fails");

--  begin read only
   end Test_Bar2;
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
end Pkh.Test_Data.Tests;
