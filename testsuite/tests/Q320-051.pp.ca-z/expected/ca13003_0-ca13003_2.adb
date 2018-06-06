--==================================================================--

separate (Ca13003_0)
package body Ca13003_2 is

   procedure Ca13003_3 is separate;                 -- Open files
   function Ca13003_4
     (Id_In : File_Id; File_In : File_Rec)
     return File_Name is separate;                  -- Process files
   package body Ca13003_5 is separate;              -- Generate report

end Ca13003_2;
