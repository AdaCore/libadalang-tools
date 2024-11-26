--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Pkg_Root.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;
with Pkg1.Stub_Data; use Pkg1.Stub_Data;
with Pkg2.Stub_Data; use Pkg2.Stub_Data;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Pkg_Root.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Foo_Root (Gnattest_T : in out Test);
   procedure Test_Foo_Root_c6e1a1 (Gnattest_T : in out Test) renames Test_Foo_Root;
--  id:2.2/c6e1a1ddadbde2a1/Foo_Root/1/0/
   procedure Test_Foo_Root (Gnattest_T : in out Test) is
   --  pkg_root.ads:3:4:Foo_Root
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

   --  Pkg1.Stub_Data.Set_Stub_Foo1_4f03c1_893a61( );
   --  Pkg2.Stub_Data.Set_Stub_Foo2_5684a5_2bc96d( );

      AUnit.Assertions.Assert
        (Foo_Root (1) = 4,
         "Stubs not properly called");

--  begin read only
   end Test_Foo_Root;
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
end Pkg_Root.Test_Data.Tests;
