pragma Warnings (Off); -- ????????????????
with Ada.Directories; use Ada.Directories;
with System.Multiprocessors;

--  with ASIS_UL.Compiler_Options;
with ASIS_UL.Options;      use ASIS_UL.Options;
--  with ASIS_UL.Projects;     use ASIS_UL.Projects;
--  with ASIS_UL.Source_Table; use ASIS_UL;

package body LAL_UL.Common.Post is

   procedure Postprocess_Common (Cmd : Command_Line) is
   begin
      null;
      --  We need to ignore -j in the inner invocation; otherwise we will
      --  complain about mixing -j with -rnb when not in --incremental mode.

      ASIS_UL.Options.Process_Num :=
        (if Mimic_gcc (Cmd) then 1 else Arg (Cmd, Jobs));
      J_Specified := ASIS_UL.Options.Process_Num /= 1;

      if ASIS_UL.Options.Process_Num = 0 then
         ASIS_UL.Options.Process_Num :=
           Positive (System.Multiprocessors.Number_Of_CPUs);
      end if;

--      ASIS_UL.Options.Outer_Parallel := Arg (Cmd, Outer_Parallel);

      if Arg (Cmd, Output_Dir) /= null then
         Out_Dir := new String'(Full_Name (Arg (Cmd, Output_Dir).all));
      end if;

--      Generate_Representation_Clauses := Arg (Cmd, Rep_Clauses);
--
--      Source_Table.Total_Sources := File_Names (Cmd)'Length;
--      Source_Table.Sources_Left  := Source_Table.Total_Sources;
--
--      Compiler_Options.Set_Arg_List;
--
--      ASIS_UL.Options.Compute_Timing := Arg (Cmd, Compute_Timing);
--
--      if Arg (Cmd, Subdirs) /= null then
--         Set_Subdir_Name (Arg (Cmd, Subdirs).all);
--      end if;
--
--      if Arg (Cmd, No_Objects_Dir) then
--         No_Object_Dir := True;
--      end if;
--
--      if Arg (Cmd, Run_Time_System).all /= "" then
--         ASIS_UL.Compiler_Options.Store_Option
--           (Arg (Cmd, Run_Time_System).all);
--      end if;
   end Postprocess_Common;

end LAL_UL.Common.Post;
