--=================================================================--

with Ca11011_0.Ca11011_1;       -- with public Child Package OS.File_Manager
with Report;

procedure Ca11011 is

   package Os renames Ca11011_0;
   package File_Manager renames Ca11011_0.Ca11011_1;

   Data_Base_File_Key : Os.File_Descriptor_Type := Os.First_File;
   Tc_Status          : Boolean                 := False;

begin

   -- This test indicates one approach to file management operations. It is not
   -- intended to demonstrate full functionality, but rather that the use of a
   -- private child package can provide a solution to a typical user situation.

   Report.Test
     ("CA11011",
      "Check that a private child package can use " &
      "entities declared in the private part of the " &
      "parent unit of its parent unit");

   Os.Verify_Initial_Conditions (Data_Base_File_Key, Tc_Status);

   if not Tc_Status then
      Report.Failed ("Initial condition failure");
   end if;

   -- Perform file initializations.

   File_Manager.Create_File (File_Key => Data_Base_File_Key);

   Tc_Status := Os.Final_Conditions_Valid (Data_Base_File_Key);

   if not Tc_Status then
      Report.Failed ("Bad status return from Create_File");
   end if;

   Report.Result;

end Ca11011;
