--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Pkg.Test_Data.

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
package body Pkg.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Foo (Gnattest_T : in out Test);
   procedure Test_Foo_bf5139 (Gnattest_T : in out Test) renames Test_Foo;
--  id:2.2/bf513967b920d3e3/Foo/1/0/
   procedure Test_Foo (Gnattest_T : in out Test) is
   --  pkg.ads:2:5:Foo
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert (True, "Always passes");

--  begin read only
   end Test_Foo;
--  end read only


--  begin read only
   procedure Test_Bar (Gnattest_T : in out Test);
   procedure Test_Bar_07cfb5 (Gnattest_T : in out Test) renames Test_Bar;
--  id:2.2/07cfb5f9d9ce2e86/Bar/1/0/
   procedure Test_Bar (Gnattest_T : in out Test) is
   --  pkg.ads:3:5:Bar
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert (True, "Always passes");

--  begin read only
   end Test_Bar;
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
end Pkg.Test_Data.Tests;
