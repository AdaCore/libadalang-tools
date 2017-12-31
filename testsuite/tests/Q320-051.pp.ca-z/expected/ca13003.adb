--==================================================================--

with Ca13003_0;
with Report;

procedure Ca13003 is
   First_File_Name  : Ca13003_0.File_Name := "Joe Smith ";
   First_File_Id    : Ca13003_0.File_Id   := 11;
   Second_File_Name : Ca13003_0.File_Name := "John Schep";
   Second_File_Id   : Ca13003_0.File_Id   := 47;
   Expected_Name    : Ca13003_0.File_Name := "          ";
   Student_File     : Ca13003_0.File_Rec;

   function Process_Input_Files (Id_In : Ca13003_0.File_Id;
      File_In : Ca13003_0.File_Rec) return Ca13003_0.File_Name renames
     Ca13003_0.Ca13003_1.Ca13003_4;

   function Process_Audit_Files (Id_In : Ca13003_0.File_Id;
      File_In : Ca13003_0.File_Rec) return Ca13003_0.File_Name renames
     Ca13003_0.Ca13003_2.Ca13003_4;
begin
   Report.Test
     ("CA13003",
      "Check that separate subunits which share " &
      "an ancestor may have the same name if they have " &
      "different fully qualified names");

   Student_File := (Id => First_File_Id, Name => First_File_Name);

   -- Note that all subunits have the same simple name. Generate report from
   -- file processing.
   Ca13003_0.Ca13003_1.Ca13003_3;
   Expected_Name := Process_Input_Files (First_File_Id, Student_File);
   Ca13003_0.Ca13003_1.Ca13003_5.Generate_Report;

   if not Ca13003_0.Tc_Open_For_Process or
     not Ca13003_0.Tc_Report_From_Process or Expected_Name /= First_File_Name
   then
      Report.Failed ("Unexpected results in processing file");
   end if;

   Ca13003_0.Initialize_File_Rec
     (Second_File_Name, Second_File_Id, Student_File);

   -- Generate report from file auditing.
   Ca13003_0.Ca13003_2.Ca13003_3;
   Expected_Name := Process_Audit_Files (Second_File_Id, Student_File);
   Ca13003_0.Ca13003_2.Ca13003_5.Generate_Report;

   if not Ca13003_0.Tc_Open_For_Audit or not Ca13003_0.Tc_Report_From_Audit or
     Expected_Name /= Second_File_Name then
      Report.Failed ("Unexpected results in auditing file");
   end if;

   Report.Result;

end Ca13003;
