--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Contracts.Test_Data.

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
package body Contracts.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_Sqrt_b564f2_b457a8 (X : Float)  return Float
   is
   begin
      declare
         Test_Sqrt_b564f2_b457a8_Result : constant Float := GNATtest_Generated.GNATtest_Standard.Contracts.Sqrt (X);
      begin
         return Test_Sqrt_b564f2_b457a8_Result;
      end;
   end Wrap_Test_Sqrt_b564f2_b457a8;
--  end read only

--  begin read only
   procedure Test_Sqrt_test_case_1 (Gnattest_T : in out Test);
   procedure Test_Sqrt_b564f2_b457a8 (Gnattest_T : in out Test) renames Test_Sqrt_test_case_1;
--  id:2.2/b564f233a5232b64/Sqrt/1/0/test_case_1/
   procedure Test_Sqrt_test_case_1 (Gnattest_T : in out Test) is
   --  contracts.ads:11:4:Sqrt
      function Sqrt (X : Float) return Float renames Wrap_Test_Sqrt_b564f2_b457a8;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Sqrt (100.0) = 10.0,
         "not 10.0");

--  begin read only
   end Test_Sqrt_test_case_1;
--  end read only

--  begin read only
   function Wrap_Test_Sqrt_b564f2_023e5f (X : Float)  return Float
   is
   begin
      declare
         Test_Sqrt_b564f2_023e5f_Result : constant Float := GNATtest_Generated.GNATtest_Standard.Contracts.Sqrt (X);
      begin
         return Test_Sqrt_b564f2_023e5f_Result;
      end;
   end Wrap_Test_Sqrt_b564f2_023e5f;
--  end read only

--  begin read only
   procedure Test_Sqrt_test_case_2 (Gnattest_T : in out Test);
   procedure Test_Sqrt_b564f2_023e5f (Gnattest_T : in out Test) renames Test_Sqrt_test_case_2;
--  id:2.2/b564f233a5232b64/Sqrt/1/0/test_case_2/
   procedure Test_Sqrt_test_case_2 (Gnattest_T : in out Test) is
   --  contracts.ads:11:4:Sqrt
      function Sqrt (X : Float) return Float renames Wrap_Test_Sqrt_b564f2_023e5f;
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Sqrt (9.0) = 3.0,
         "not 3.0");

--  begin read only
   end Test_Sqrt_test_case_2;
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
end Contracts.Test_Data.Tests;
