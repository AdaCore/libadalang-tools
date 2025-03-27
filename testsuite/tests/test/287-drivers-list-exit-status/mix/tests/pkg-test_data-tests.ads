--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with Gnattest_Generated;
with AUnit.Test_Caller;

package Pkg.Test_Data.Tests is

   type Test is new GNATtest_Generated.GNATtest_Standard.Pkg.Test_Data.Test
   with null record;

   procedure Test_Foo_bf5139 (Gnattest_T : in out Test);
   --  pkg.ads:2:5:Foo

   procedure Test_Bar_07cfb5 (Gnattest_T : in out Test);
   --  pkg.ads:3:5:Bar

   package Caller is new AUnit.Test_Caller (Test);

end Pkg.Test_Data.Tests;
--  end read only
