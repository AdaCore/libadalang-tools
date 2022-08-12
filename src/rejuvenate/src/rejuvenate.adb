------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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
--  Rejuvenate tools driver

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Libadalang.Analysis;
with Libadalang.Project_Provider;
with Laltools.Refactor;
with VSS.Stream_Element_Vectors.Conversions;
with VSS.Text_Streams.Memory_UTF8_Output;
--  with Ada.Exceptions; use Ada.Exceptions;

with GNATCOLL.Strings; use GNATCOLL.Strings;
with GNATCOLL.Projects;

with Command_Line;
with GNATCOLL.VFS;

with Tools;
with Tools.Record_Components_Tool;
with Tools.Array_Aggregates_Tool;
with Tools.Suppress_Dead_Params_Tool;
with Tools.Scope_Declarations_Tool;
with Tools.Relocate_Decls_Tool;
with Edit_File;

procedure Rejuvenate is
   package Project_Tree renames Command_Line.Project;
   package LAL renames Libadalang.Analysis;
   package GPR renames GNATCOLL.Projects;
   package LAL_GPR renames Libadalang.Project_Provider;
   package ReFac renames Laltools.Refactor;
   Env     : GPR.Project_Environment_Access;
   Project : constant GPR.Project_Tree_Access := new GPR.Project_Tree;

   use type GNATCOLL.VFS.Filesystem_String;
   function "+" (Str : String) return XString renames To_XString;
   --  TODO: Move this to an utils package

   First_Tool_Index : constant Natural := Tools.Find_First_Tool_Index;

   Command_Line_Arguments : XString_Array :=
     (if First_Tool_Index = 0 then []
      else [1 .. Argument_Count => +""]);
   Context          : LAL.Analysis_Context;
   Provider         : LAL.Unit_Provider_Reference;
   Save_Dir         : Ada.Strings.Unbounded.Unbounded_String;
   File             : File_Type;
   Stream           : aliased VSS.Text_Streams.Memory_UTF8_Output
     .Memory_UTF8_Output_Stream;
   Edit_Map         : ReFac.Text_Edit_Map;

   --  TODO: Hide the Command_Line_Arguments and Tool_Args logic.
   --  TODO: Rename Command_Line_Arguments since this represents the
   --  arguments that common to all tools. Possible new name: Common_Arguments.

begin
   for J in 1 ..  Argument_Count loop
      Command_Line_Arguments (J) := +Argument (J);
   end loop;

   if Command_Line.Parser.Parse (Command_Line_Arguments) then
      declare
         Project_Filename : constant String :=
                 To_String (Project_Tree.Get);
         My_Project_File  : constant GNATCOLL.VFS.Virtual_File :=
           GNATCOLL.VFS.Create (+Project_Filename);
         Source_Files : LAL_GPR.Filename_Vectors.Vector;
      begin
         GPR.Initialize (Env);
               --  Use procedures in GNATCOLL.Projects to set scenario
               --  variables (Change_Environment), to set the target
               --  and the runtime (Set_Target_And_Runtime), etc.
         Project.Load (My_Project_File, Env);
         Provider := LAL_GPR.Create_Project_Unit_Provider
           (Tree => Project, Env => Env);

         Context := LAL.Create_Context (Unit_Provider => Provider);
         if Command_Line.Source.Get /= Null_Unbounded_String then
            Source_Files := Libadalang.Project_Provider.Source_Files
                 (Project.all, Libadalang.Project_Provider.Whole_Project);
         else
            Source_Files := Libadalang.Project_Provider.Source_Files
              (Project.all, Libadalang.Project_Provider.Root_Project);
            --  TODO : if the user want to specify the source
         end if;
         declare
            AUA : LAL.Analysis_Unit_Array (Source_Files.First_Index
                                                 .. Source_Files.Last_Index);
         begin
            for I in Source_Files.First_Index .. Source_Files.Last_Index loop
               declare
                  Unit     : constant LAL.Analysis_Unit :=
                    Context.Get_From_File (To_String
                                           (Source_Files.Element (I)));
               begin
                  --  Report parsing errors, if any
                  if Unit.Has_Diagnostics then
                     for D of Unit.Diagnostics loop
                        Put_Line (Unit.Format_GNU_Diagnostic (D));
                     end loop;
                     --  Otherwise, look for object declarations
                  else
                     AUA (I) := Unit;
                  end if;
               end;
            end loop;
            case Command_Line.Tool.Get is
                  when Tools.Record_Components =>
                     Tools.Record_Components_Tool.Run (AUA, Stream);
                  when Tools.Array_Aggregates =>
                     Tools.Array_Aggregates_Tool.Run (AUA, Stream);
                  when Tools.Suppress_Dead_Params =>
                     Tools.Suppress_Dead_Params_Tool.Run (AUA, Stream);
                  when Tools.Scope_Declarations =>
                     Tools.Scope_Declarations_Tool.Run (AUA, Stream);
                  when Tools.Relocate_Decls =>
                     Tools.Relocate_Decls_Tool.Run (AUA, Stream);
            end case;
         end;
         if Command_Line.Pipe.Get then
            Put_Line (VSS.Stream_Element_Vectors.Conversions
                         .Unchecked_To_String (Stream.Buffer));
         end if;
         if Command_Line.Interactive.Get then
            case Command_Line.Tool.Get is
               when Tools.Record_Components =>
                  --  Tools.Record_Components_Tool.Interact;
                  null;
               when Tools.Array_Aggregates =>
                  --  Tools.Array_Aggregates_Tool.Interact;
                  null;
               when Tools.Suppress_Dead_Params =>
                  --  Tools.Suppress_Dead_Params_Tool.Interact;
                  null;
               when Tools.Scope_Declarations =>
                  Edit_Map := Tools.Scope_Declarations_Tool.Interact;
               when Tools.Relocate_Decls =>
                  --  Tools.Relocate_Decls_Tool.Interact;
                  null;
            end case;
         end if;
         Save_Dir := Command_Line.Output_Dir.Get;
         if Save_Dir /= Ada.Strings.Unbounded.Null_Unbounded_String then
            Edit_File.Apply_Edits (Edit_Map, To_String (Save_Dir));
         else
            Edit_File.Apply_Edits (Edit_Map, ".");
         end if;
      end;
   end if;
--  GNAT bug
--  exception
--     when Parse_Tool_Exception =>
--        Ada.Text_IO.Put_Line (Args.Parser.Help);
--     when others =>
--        Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
--  TODO: Raise a ticket with this bug and ammend this comment with the
--  ticket number.
end Rejuvenate;
