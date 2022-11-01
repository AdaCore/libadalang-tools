--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Speed1.Controller_Test_Data.

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
package body Speed1.Controller_Test_Data.Controller_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Speed (Gnattest_T : in out Test_Controller);
   procedure Test_Speed_bdc804 (Gnattest_T : in out Test_Controller) renames Test_Speed;
--  id:2.2/bdc8045e732efa1b/Speed/1/0/
   procedure Test_Speed (Gnattest_T : in out Test_Controller) is
   --  speed1.ads:12:4:Speed
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Speed;
--  end read only


--  begin read only
   procedure Test_Adjust_Speed (Gnattest_T : in out Test_Controller);
   procedure Test_Adjust_Speed_6fd48f (Gnattest_T : in out Test_Controller) renames Test_Adjust_Speed;
--  id:2.2/6fd48ff933c1edff/Adjust_Speed/1/0/
   procedure Test_Adjust_Speed (Gnattest_T : in out Test_Controller) is
   --  speed1.ads:13:4:Adjust_Speed
--  end read only
   begin
      Gnattest_T.Fixture.Actual_Speed := 0;
      Gnattest_T.Fixture.Adjust_Speed (4);
      Assert (Gnattest_T.Fixture.Speed = 4,
              "wrong speed after first assignment:" &
                Integer'Image (Gnattest_T.Fixture.Speed));
      Gnattest_T.Fixture.Adjust_Speed (-2);
      Assert (Gnattest_T.Fixture.Speed = 2,
              "wrong speed after second assignment" &
                Integer'Image (Gnattest_T.Fixture.Speed));

--  begin read only
   end Test_Adjust_Speed;
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
end Speed1.Controller_Test_Data.Controller_Tests;
