--=================================================================--

with Ca11003_0.Ca11003_1.Ca11003_2;  -- Grandchild Pkg OS.Operations.Menu
with Ca11003_0.Ca11003_1.Ca11003_3;  -- Grandchild Ftn OS.Operations.Validate
with Report;

procedure Ca11003 is

   package Menu renames Ca11003_0.Ca11003_1.Ca11003_2;

begin

   Report.Test
     ("CA11003",
      "Check that a public grandchild can utilize " &
      "its ancestor unit's visible definitions");

   File_Processing :         -- Validate all of the capabilities contained in
   -- the Menu package by exercising them on specific files. This will
      -- demonstrate the use of child and grandchild functionality based on
      -- components that have been declared in the parent/grandparent package.
      declare

      function Validate
        (File : Ca11003_0.Ca11003_1.Extended_File_Type) return Boolean renames
        Ca11003_0.Ca11003_1.Ca11003_3;

      Macwrite_File, Backup_Copy : Ca11003_0.Ca11003_1.Extended_File_Type;
      Macwrite_File_Mode         : Ca11003_0.File_Mode := Ca11003_0.Read_Write;

   begin

      Menu.News (Macwrite_File_Mode, Macwrite_File);

      if not Validate (Macwrite_File) then
         Report.Failed ("Incorrect initialization of files");
      end if;

      Menu.Copy (Macwrite_File, Backup_Copy);

      if not (Validate (Macwrite_File) and Validate (Backup_Copy)) then
         Report.Failed ("Incorrect duplication of files");
      end if;

      Menu.Delete (Backup_Copy);

   exception
      when Ca11003_0.File_Data_Error =>
         Report.Failed ("Exception raised during file validation");
      when Ca11003_0.Ca11003_1.File_Duplication_Error =>
         Report.Failed ("Exception raised during file duplication");
      when others =>
         Report.Failed ("Unexpected exception in test procedure");

   end File_Processing;

   Report.Result;

end Ca11003;
