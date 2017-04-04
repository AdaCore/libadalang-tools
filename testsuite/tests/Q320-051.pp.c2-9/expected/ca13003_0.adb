--==================================================================--

package body Ca13003_0 is

   procedure Initialize_File_Rec
     (Name_In : in     File_Name;
      Id_In   : in     File_Id;
      File_In :    out File_Rec)
   is
   -- Not a real initialization. Real application can use file database to
   -- create the file record.
   begin
      File_In.Name := Name_In;
      File_In.Id   := Id_In;
   end Initialize_File_Rec;

   package body Ca13003_1 is separate;
   package body Ca13003_2 is separate;

end Ca13003_0;
