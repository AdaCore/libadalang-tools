--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into P.T_Test_Data.

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
package body P.T_Test_Data.T_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_1_Do_Stuff (Gnattest_T : in out Test_T);
   procedure Test_Do_Stuff_900ee1 (Gnattest_T : in out Test_T) renames Test_1_Do_Stuff;
--  id:2.2/900ee10e14992be0/Do_Stuff/1/0/
   procedure Test_1_Do_Stuff (Gnattest_T : in out Test_T) is
   --  p.ads:4:4:Do_Stuff
--  end read only

   begin

      Gnattest_T.Fixture.X := 0;
      AUnit.Assertions.Assert
        (Do_Stuff (Gnattest_T.Fixture.all) = 1,
         "Do_Stuff is wrong for T");

--  begin read only
   end Test_1_Do_Stuff;
--  end read only


--  begin read only
   procedure Test_Do_Other_Stuff (Gnattest_T : in out Test_T);
   procedure Test_Do_Other_Stuff_c5df94 (Gnattest_T : in out Test_T) renames Test_Do_Other_Stuff;
--  id:2.2/c5df949e067e4301/Do_Other_Stuff/1/0/
   procedure Test_Do_Other_Stuff (Gnattest_T : in out Test_T) is
   --  p.ads:5:4:Do_Other_Stuff
--  end read only

   begin

      Gnattest_T.Fixture.X := 0;
      AUnit.Assertions.Assert
        (Do_Other_Stuff (Gnattest_T.Fixture.all) = 2,
         "Do_Other_Stuff is wrong for T");

--  begin read only
   end Test_Do_Other_Stuff;
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
end P.T_Test_Data.T_Tests;
