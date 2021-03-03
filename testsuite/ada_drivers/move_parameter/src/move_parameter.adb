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
--  Move Parameter Tool
--
--  This tool will move a parameter forward or backward, if possible.
--
--  Usage:
--  remove_parameter -P <project_file> -S <source_code_file> -L <line_number>
--  -R <column_number> -D <direction>
--
--  -P, --project          Project file to use
--  -S, --source           Source code file of the node
--  -L, --line             Line number of the node
--  -R, --column           Column number of the node
--  -D, --direction        Move direction: 'left' or 'right'

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text; use Langkit_Support.Text;

with Laltools.Common; use Laltools.Common;
with Laltools.Refactor; use Laltools.Refactor;
with Laltools.Refactor.Subprogram_Signature;
use Laltools.Refactor.Subprogram_Signature;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;

procedure Move_Parameter is

   procedure Move_Parameter_App_Setup
     (Context : App_Context;
      Jobs : App_Job_Context_Array);
   --  Main procedure of this program.

   function Image (D : Move_Direction_Type) return String;
   --  Returns 'forward' if D = Forward and 'backward' if D = Backward

   package Move_Parameter_App is new App
     (Name             => "move_parameter",
      Description      => "Move Parameter",
      App_setup        => Move_Parameter_App_Setup);

   package Source is new Parse_Option
     (Parser      => Move_Parameter_App.Args.Parser,
      Short       => "-S",
      Long        => "--source",
      Help        => "Source code file of the node",
      Arg_Type    => Unbounded_String,
      Convert     => To_Unbounded_String,
      Default_Val => Null_Unbounded_String,
      Enabled     => True);

   package Line is new Parse_Option
     (Parser      => Move_Parameter_App.Args.Parser,
      Short       => "-L",
      Long        => "--line",
      Help        => "Line of the node",
      Arg_Type    => Natural,
      Convert     => Natural'Value,
      Default_Val => 1,
      Enabled     => True);

   package Column is new Parse_Option
     (Parser      => Move_Parameter_App.Args.Parser,
      Short       => "-R",
      Long        => "--column",
      Help        => "Column of the node",
      Arg_Type    => Natural,
      Convert     => Natural'Value,
      Default_Val => 1,
      Enabled     => True);

   package Direction is new GNATCOLL.Opt_Parse.Parse_Option
     (Parser      => Move_Parameter_App.Args.Parser,
      Short       => "-D",
      Long        => "--direction",
      Help        => "Move direction: 'forward' or 'backward'",
      Arg_Type    => Unbounded_String,
      Convert     => To_Unbounded_String,
      Default_Val => Null_Unbounded_String,
      Enabled     => True);

   -----------
   -- Image --
   -----------

   function Image (D : Move_Direction_Type) return String is
   begin
      case D is
         when Forward  => return "forward";
         when Backward => return "backward";
      end case;
   end Image;

   ------------------------------
   -- Move_Parameter_App_Setup --
   ------------------------------

   procedure Move_Parameter_App_Setup
     (Context : App_Context;
      Jobs : App_Job_Context_Array)
   is
      Source_File : constant Unbounded_String := Source.Get;

      Sloc : constant Source_Location :=
        (Line_Number (Line.Get), Column_Number (Column.Get));

      Direction_String : constant Unbounded_String := Direction.Get;
      Move_Direction   : Move_Direction_Type;

      Files : constant File_Array_Access :=
        Context.Provider.Project.Root_Project.Source_Files;

      Main_Unit       : Analysis_Unit;
      Node            : Ada_Node;
      Number_Of_Units : constant Positive := Files'Length;
      Units_Index     : Positive := 1;
      Units           : Analysis_Unit_Array (1 .. Number_Of_Units);

      function Analysis_Units return Analysis_Unit_Array is (Units);

      Target_Subp            : Basic_Decl;
      Target_Parameter_Index : Positive;
      Available_Directions   : Move_Direction_Availability_Type;

      Edits : Refactoring_Edits;

   begin
      if Ada.Strings.Unbounded.Equal_Case_Insensitive
        (Direction_String, To_Unbounded_String ("forward"))
      then
         Move_Direction := Forward;

      elsif Ada.Strings.Unbounded.Equal_Case_Insensitive
        (Direction_String, To_Unbounded_String ("backward"))
      then
         Move_Direction := Backward;

      else
         Put_Line
           ("Invalid direction argument (-D, --direction)."
            & "Valid direction arguments are 'forward' and 'backward'");
         New_Line;
         return;
      end if;

      Main_Unit := Jobs (1).Analysis_Ctx.Get_From_File
        (To_String (Source_File));

      Node := Main_Unit.Root.Lookup (Sloc);

      for File of Files.all loop
         declare
            Filename : constant Filesystem_String := File.Full_Name;

         begin
            Units (Units_Index) :=
              Node.Unit.Context.Get_From_File (String (Filename));
            Units_Index := Units_Index + 1;
         end;
      end loop;

      if Is_Move_Parameter_Available
        (Node, Target_Subp, Target_Parameter_Index, Available_Directions)
      then
         if Available_Directions (Move_Direction) then
            Put_Line
              ("Moving parameter "
               & Image (Get_Parameter_Name
                 (Target_Subp, Target_Parameter_Index))
               & " "
               & Image (Move_Direction));

            case Move_Direction is
               when Forward =>
                  declare
                     Mover : constant Forward_Mover := Create
                       (Target_Subp, Target_Parameter_Index);

                  begin
                     Edits := Mover.Refactor (Analysis_Units'Access);
                  end;

               when Backward =>
                  declare
                     Mover : constant Backward_Mover := Create
                       (Target_Subp, Target_Parameter_Index);

                  begin
                     Edits := Mover.Refactor (Analysis_Units'Access);
                  end;
            end case;

            Print (Edits.Text_Edits);

         else
            Put_Line
              (Image (Get_Parameter_Name (Target_Subp, Target_Parameter_Index))
               & " cannot be moved "
               & Image (Move_Direction));
            New_Line;
         end if;

      else
         Put_Line
           ("Not possible to move any parameter given node "
            & Node.Image);
         New_Line;
      end if;
   end Move_Parameter_App_Setup;

begin
   Move_Parameter_App.Run;
end Move_Parameter;
