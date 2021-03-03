------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
-- Libadalang Tools  is free software; you can redistribute it and/or modi- --
-- fy  it  under  terms of the  GNU General Public License  as published by --
-- the Free Software Foundation;  either version 3, or (at your option) any --
-- later version. This software  is distributed in the hope that it will be --
-- useful but  WITHOUT  ANY  WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                  --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------
--
--  Change Parameter Mode Tool
--
--  This tool will the mode of a parameter.
--
--  Usage:
--  remove_parameter -P <project_file> -S <source_code_file> -L <line_number>
--  -R <column_number> -D <direction>
--
--  -P, --project          Project file to use
--  -S, --source           Source code file of the node
--  -L, --line             Line number of the node
--  -R, --column           Column number of the node
--  -M, --mode             New parameter mode

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;

with Laltools.Common; use Laltools.Common;
with Laltools.Refactor; use Laltools.Refactor;
with Laltools.Refactor.Subprogram_Signature;
use Laltools.Refactor.Subprogram_Signature;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;
with Libadalang.Helpers; use Libadalang.Helpers;

procedure Change_Parameter_Mode is

   Main_Unit : Analysis_Unit;
   Node      : Ada_Node;

   procedure Change_Parameter_Mode_App_Setup
     (Context : App_Context;
      Jobs : App_Job_Context_Array);

   package Change_Parameter_Mode_App is new App
     (Name             => "change_parameter_mode",
      Description      => "Change Parameter Mode",
      App_setup        => Change_Parameter_Mode_App_Setup);

   package Source is new Parse_Option
     (Parser      => Change_Parameter_Mode_App.Args.Parser,
      Short       => "-S",
      Long        => "--source",
      Help        => "Source code file of the node",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String,
      Default_Val => Ada.Strings.Unbounded.Null_Unbounded_String,
      Enabled     => True);

   package Line is new GNATCOLL.Opt_Parse.Parse_Option
     (Parser      => Change_Parameter_Mode_App.Args.Parser,
      Short       => "-L",
      Long        => "--line",
      Help        => "Line of the node",
      Arg_Type    => Natural,
      Convert     => Natural'Value,
      Default_Val => 1,
      Enabled     => True);

   package Column is new GNATCOLL.Opt_Parse.Parse_Option
     (Parser      => Change_Parameter_Mode_App.Args.Parser,
      Short       => "-R",
      Long        => "--column",
      Help        => "Column of the node",
      Arg_Type    => Natural,
      Convert     => Natural'Value,
      Default_Val => 1,
      Enabled     => True);

   package Mode is new GNATCOLL.Opt_Parse.Parse_Option
     (Parser      => Change_Parameter_Mode_App.Args.Parser,
      Short       => "-M",
      Long        => "--mode",
      Help        => "New mode",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String,
      Default_Val => Ada.Strings.Unbounded.Null_Unbounded_String,
      Enabled     => True);

   -------------------------------------
   -- Change_Parameter_Mode_App_Setup --
   -------------------------------------

   procedure Change_Parameter_Mode_App_Setup
     (Context : App_Context;
      Jobs : App_Job_Context_Array)
   is
      Source_File : constant Unbounded_String := Source.Get;

      Sloc : constant Source_Location :=
        (Line   => Line_Number (Line.Get),
         Column => Column_Number (Column.Get));

      New_Mode_String : constant Unbounded_String := Mode.Get;
      New_Mode : Ada_Mode;

      Found : Boolean := False;

      Files : constant File_Array_Access :=
        Context.Provider.Project.Root_Project.Source_Files;

      Number_Of_Units : constant Positive := Files'Length;
      Units_Index     : Positive := 1;
      Units           : Analysis_Unit_Array (1 .. Number_Of_Units);

      function Analysis_Units return Analysis_Unit_Array is (Units);

      Target_Subp               : Basic_Decl;
      Target_Parameters_Indices : Parameter_Indices_Range_Type;
      Mode_Alternatives         : Mode_Alternatives_Type;

      Mode_Changer_Refactoring : Mode_Changer;

      Parameters_Names : Langkit_Support.Text.Unbounded_Text_Type;

      Edits : Refactoring_Edits;

      use type Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;

      function Image (M : Ada_Mode) return String;
      --  TODO

      function Image (M : Ada_Mode) return String is
      begin
         case M is
            when Ada_Mode_In      => return "in";
            when Ada_Mode_Out     => return "out";
            when Ada_Mode_In_Out  => return "in out";
            when Ada_Mode_Default => return "default";
         end case;
      end Image;

   begin
      Main_Unit := Jobs (1).Analysis_Ctx.Get_From_File
        (Ada.Strings.Unbounded.To_String (Source_File));

      Node := Main_Unit.Root.Lookup (Sloc);

      for File of Files.all loop
         declare
            Filename : constant GNATCOLL.VFS.Filesystem_String :=
              File.Full_Name;

         begin
            Units (Units_Index) :=
              Node.Unit.Context.Get_From_File (String (Filename));
            Units_Index := Units_Index + 1;
         end;
      end loop;

      if Ada.Strings.Unbounded.Equal_Case_Insensitive
        (New_Mode_String, To_Unbounded_String ("default"))
      then
         New_Mode := Ada_Mode_Default;
      elsif Ada.Strings.Unbounded.Equal_Case_Insensitive
        (New_Mode_String, To_Unbounded_String ("in"))
      then
         New_Mode := Ada_Mode_In;
      elsif Ada.Strings.Unbounded.Equal_Case_Insensitive
        (New_Mode_String, To_Unbounded_String ("out"))
      then
         New_Mode := Ada_Mode_Out;
      elsif Ada.Strings.Unbounded.Equal_Case_Insensitive
        (New_Mode_String, To_Unbounded_String ("in out"))
      then
         New_Mode := Ada_Mode_In_Out;
      else
         raise Program_Error;
      end if;

      if Is_Change_Mode_Available
        (Node, Target_Subp, Target_Parameters_Indices, Mode_Alternatives)
      then
         for Alternative of Mode_Alternatives loop
            if New_Mode = Alternative then
               Found := True;
               exit;
            end if;
         end loop;

         if not Found then
            Put_Line
              ("Not possible to change the parameter mode to"
               & New_Mode'Image);
            return;
         end if;

         if Target_Parameters_Indices.First /=
           Target_Parameters_Indices.Last
         then
            for Index in Target_Parameters_Indices.First ..
              Target_Parameters_Indices.Last - 1
            loop
               Ada.Strings.Wide_Wide_Unbounded.Append
                 (Parameters_Names, Langkit_Support.Text.To_Unbounded_Text
                    (Get_Parameter_Name (Target_Subp, Index))
                  & ", ");
            end loop;

            Parameters_Names := Parameters_Names
              & Langkit_Support.Text.To_Unbounded_Text
              (Get_Parameter_Name
                 (Target_Subp, Target_Parameters_Indices.Last));
         else
            Parameters_Names := Langkit_Support.Text.To_Unbounded_Text
              (Get_Parameter_Name
                 (Target_Subp, Target_Parameters_Indices.First));
         end if;

         if Target_Parameters_Indices.First =
           Target_Parameters_Indices.Last
         then
            Put_Line
              ("Changing mode of parameter "
               & Langkit_Support.Text.Image
                 (Langkit_Support.Text.To_Text (Parameters_Names))
               & " to "
               & Image (New_Mode));

         else
            Put_Line
              ("Changing mode of parameters "
               & Langkit_Support.Text.Image
                 (Langkit_Support.Text.To_Text (Parameters_Names))
               & " to "
               & Image (New_Mode));
         end if;

         Mode_Changer_Refactoring :=
           Create
             (Target_Subp,
              Target_Parameters_Indices,
              New_Mode);

         Edits := Mode_Changer_Refactoring.Refactor (Analysis_Units'Access);

         Print (Edits.Text_Edits);

      else
         Put_Line
           ("Not possible to change the parameter mode of any parameter "
            & "given node "
            & Node.Image);
      end if;
   end Change_Parameter_Mode_App_Setup;

begin
   Change_Parameter_Mode_App.Run;
end Change_Parameter_Mode;
