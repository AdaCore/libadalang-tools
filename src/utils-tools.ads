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

with GNATCOLL.Projects;

with Libadalang.Analysis; use Libadalang.Analysis;

with Utils.Command_Lines; use Utils.Command_Lines;

package Utils.Tools is

   --  Each tool should derive from Tool_State, and override the ops.
   --  The driver calls Init, then Per_File_Action on each source file,
   --  then Final.

   type Tool_State is abstract tagged limited record
      Project_Tree : GNATCOLL.Projects.Project_Tree_Access;
      --  The driver sets this to the current project. If there is no
      --  -P switch, then the Status will be Empty.

      Project_Env : GNATCOLL.Projects.Project_Environment_Access;

      Context : Analysis_Context := No_Analysis_Context;
      --  The only tool that needs access to the Context is gnatstub.
   end record;

   procedure Init (Tool : in out Tool_State; Cmd : in out Command_Line)
      is abstract;
   procedure Per_File_Action
     (Tool : in out Tool_State;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Unit : Analysis_Unit)
     is abstract;
   --  Input is the contents of the file named by File_Name.
   --  BOM_Seen is True if there was a BOM at the start of the file;
   --  the BOM is not included in Input.

   procedure Per_Invalid_File_Action
     (Tool : in out Tool_State;
      Cmd : Command_Line;
      File_Name : String) is null;
   --  Called for invalid sources that don't make it to Per_File_Action

   procedure Process_File
     (Tool         : in out Tool_State'Class;
      Cmd          : in out Command_Line;
      File_Name    : String;
      Counter      : Natural;
      Syntax_Error : out Boolean;
      Reparse      : Boolean := False);
   --  This class-wide procedure takes care of some bookkeeping, and then
   --  dispatches to Per_File_Action.
   --
   --  If Tool.Context is nil, Process_File creates it. This is necessary
   --  because we have to defer the Create_Context call until after we've read
   --  the first file, because it might set the Wide_Character_Encoding via the
   --  BOM. This makes the somewhat questionable assumption that all files have
   --  the same encoding (which is necessary anyway if it's controlled by the
   --  command line).
   --
   --  Counter is a count of the number of files left to process. This is used
   --  to call Create_Context every N files, for some arbitrary N. Without
   --  that, we use up huge amounts of memory when processing a lot of files,
   --  due to caching in libadalang. But we don't want to call Create_Context
   --  on every file, because that slows down processing a lot.
   --
   --  Reparse has the same meaning as the parameter of Get_From_File. The
   --  reason this is needed is documented in Stub.Actions (search for the call
   --  to Process_File).

   procedure Final (Tool : in out Tool_State; Cmd : Command_Line) is abstract;
   procedure Tool_Help (Tool : Tool_State) is abstract;

end Utils.Tools;
