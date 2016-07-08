pragma Warnings (Off);

with Ada.Exceptions;

with LAL_UL.Command_Lines; use LAL_UL.Command_Lines; -- ????????????????
with LAL_UL.Projects;      use LAL_UL.Projects;
with LAL_UL.Common;        use LAL_UL.Common;
with LAL_UL.Test_Pkg;      use LAL_UL.Test_Pkg;
with GNAT.OS_Lib;          use GNAT.OS_Lib; -- ????????????????

--  with LAL_UL.Common.Post;
--  with LAL_UL.Driver;

procedure LAL_UL.Test is
   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;
   use Some_Flags_Switches, Some_Boolean_Switches, My_Enum_Switches;
   Cmd : Command_Line (Descriptor => Descriptor'Access);

   procedure Do_Nothing (Cmd : Command_Line) is null;

   procedure Print_Help;
   procedure Print_Help is
   begin
      Put_Line ("Help!");
   end Print_Help;

begin
   Put_Line ("Hello");
   declare
      Cmd_Text, Cmd_Cargs, Project_Switches_Text : Argument_List_Access;
      Global_Report_Dir               : String_Ref;
      Compiler_Options                : String_List_Access;
      Project_RTS                     : String_Access;
      Individual_Source_Options       : String_String_List_Map;
      Result_Dirs                     : String_String_Map;
   begin
      Process_Command_Line
        (Cmd,
         Cmd_Text,
         Cmd_Cargs,
         Project_Switches_Text,
         Global_Report_Dir,
         Compiler_Options,
         Project_RTS,
         Individual_Source_Options,
         Result_Dirs,
         Needs_Per_File_Output  => True,
         Tool_Package_Name      => "metrics",
         Post_Cmd_Line_1_Action => Do_Nothing'Access,
         Tool_Temp_Dir          => ".",
         Print_Help             => Print_Help'Access);
      --  ????????????????Enable => (1 => To_All (Debug_Flag_A)));
   exception
      when X : Command_Line_Error =>
         Put_Line (Ada.Exceptions.Exception_Message (X));
         --  ????????????????Try_Help.
         OS_Exit (-1);
   end;

   Dump_Cmd (Cmd, Verbose => False);
   if Arg (Cmd, Do_This) then
      Put_Line (Switch_Text (Descriptor, To_All (Do_This)).all);
   end if;
   if Arg (Cmd, Do_That) then
      Put_Line (Switch_Text (Descriptor, To_All (Do_That)).all);
   end if;
   if Arg (Cmd, Do_The_Other_Thing) then
      Put_Line (Switch_Text (Descriptor, To_All (Do_The_Other_Thing)).all);
   end if;

   Put_Line (Switch_Text (Descriptor, To_All (My_Enum'(Arg (Cmd)))).all);
   Put_Line
     (Switch_Text (Descriptor, To_All (Jobs)).all &
      " = " &
      Arg (Cmd, Jobs)'Img);

   if Arg (Cmd, Output_Dir) = null then
      Put_Line ("No " & Switch_Text (Descriptor, To_All (Output_Dir)).all);
   else
      Put_Line
        (Switch_Text (Descriptor, To_All (Output_Dir)).all &
         " = " &
         Arg (Cmd, Output_Dir).all);
   end if;
   Put_Line ("Goodbye");
   Put_Line ("");
end LAL_UL.Test;
