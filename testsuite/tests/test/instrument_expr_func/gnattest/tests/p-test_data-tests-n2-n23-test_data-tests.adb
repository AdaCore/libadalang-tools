--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into P.Test_Data.Tests.N2.N23.Test_Data.

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
package body P.Test_Data.Tests.N2.N23.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Expr_Nested_no_package_Body_2 (Gnattest_T : in out Test);
   procedure Test_Expr_Nested_no_package_Body_2_66da08 (Gnattest_T : in out Test) renames Test_Expr_Nested_no_package_Body_2;
--  id:2.2/66da0802ea936761/Expr_Nested_no_package_Body_2/1/0/
   procedure Test_Expr_Nested_no_package_Body_2 (Gnattest_T : in out Test) is
   --  p.ads:23:10:Expr_Nested_no_package_Body_2
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (P.N2.N23.Expr_Nested_no_package_Body_2 (997) = 1000,
         "worng Expr_Nested_no_package_Body");

--  begin read only
   end Test_Expr_Nested_no_package_Body_2;
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
end P.Test_Data.Tests.N2.N23.Test_Data.Tests;
