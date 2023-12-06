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
   procedure Test_Expr (Gnattest_T : in out Test);
   procedure Test_Expr_617926 (Gnattest_T : in out Test) renames Test_Expr;
--  id:2.2/6179266ae349cdc0/Expr/1/0/
   procedure Test_Expr (Gnattest_T : in out Test) is
   --  p.ads:3:4:Expr
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Expr (100) = 1000,
         "wrong expr");

--  begin read only
   end Test_Expr;
--  end read only


--  begin read only
   procedure Test_Expr_Private_Body (Gnattest_T : in out Test);
   procedure Test_Expr_Private_Body_30624e (Gnattest_T : in out Test) renames Test_Expr_Private_Body;
--  id:2.2/30624e53e675d71b/Expr_Private_Body/1/0/
   procedure Test_Expr_Private_Body (Gnattest_T : in out Test) is
   --  p.ads:6:4:Expr_Private_Body
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Expr_Private_Body (107) = 104,
         "wrong expr2");

--  begin read only
   end Test_Expr_Private_Body;
--  end read only


--  begin read only
   procedure Test_Package_Needs_Body (Gnattest_T : in out Test);
   procedure Test_Package_Needs_Body_1c64ee (Gnattest_T : in out Test) renames Test_Package_Needs_Body;
--  id:2.2/1c64eefbe84ddedf/Package_Needs_Body/1/0/
   procedure Test_Package_Needs_Body (Gnattest_T : in out Test) is
   --  p.ads:8:4:Package_Needs_Body
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (True,
         "Test not implemented.");

--  begin read only
   end Test_Package_Needs_Body;
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
