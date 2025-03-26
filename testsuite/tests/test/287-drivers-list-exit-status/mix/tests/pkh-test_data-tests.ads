--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with Gnattest_Generated;
with AUnit.Test_Caller;

package Pkh.Test_Data.Tests is

   type Test is new GNATtest_Generated.GNATtest_Standard.Pkh.Test_Data.Test
   with null record;

   procedure Test_Foo2_9e0267 (Gnattest_T : in out Test);
   --  pkh.ads:2:5:Foo2

   procedure Test_Bar2_6e99fa (Gnattest_T : in out Test);
   --  pkh.ads:3:5:Bar2

   package Caller is new AUnit.Test_Caller (Test);

end Pkh.Test_Data.Tests;
--  end read only
