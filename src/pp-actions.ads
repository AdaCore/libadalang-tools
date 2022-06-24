------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2021-2022, AdaCore                   --
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

with Libadalang.Analysis; use Libadalang.Analysis;
with Utils.Char_Vectors; use Utils.Char_Vectors;
use Utils.Char_Vectors.Char_Vectors;
with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Tools; use Utils.Tools;
with Pp.Scanner;

package Pp.Actions is

   type Pp_Tool is new Tool_State with private;

   procedure Format_Vector
     (Cmd            : Command_Line;
      Input          : Char_Vector;
      Node           : Ada_Node;
      Output         : out Char_Vector;
      Messages       : out Pp.Scanner.Source_Message_Vector;
      Partial_Gnatpp : Boolean := False) with
     Pre => Pp.Scanner.Source_Message_Vectors.Is_Empty (Messages);
   --  This pretty prints the given source. Parameters:
   --
   --     Cmd -- processed command line, or "command line" concocted by a tool
   --     such as GPS.
   --
   --     Input -- input source text. Can be empty if we are not processing an
   --     entire file. When called from GPS, this should be the entire file
   --     contents, even if In_Range indicates a smaller region to format.
   --
   --     Node -- Root of the Ada tree to format.
   --
   --     In_Range -- Range of text in Input that is relevant to the client.
   --
   --     Output -- formatted text. When called from GPS, this will be the
   --     entire whole-file result; it can use Out_Range to snip out the
   --     part that should be formatted.
   --
   --     Out_Range -- Range of text in Output that corresponds to In_Range.
   --
   --     Messages -- Error messages.
   --
   --     Partial_Gnatpp -- Boolean parameterset when Format_Vector is called
   --     in a context of code snippet reformatting (from partial-gnatpp).
   --
   --  If Messages is not empty, then the client should notify the user
   --  somehow, and avoid updating the user's source code. In addition, if
   --  Format_Vector raises an exception, that is a bug, and the client
   --  should avoid updating the user's source code.
   --
   --  If the portion of Output indicated by Out_Range is identical to the
   --  portion of Input indicated by In_Range, then clients should normally
   --  avoid changing the user's source code.
   --
   --  Note that the gnatpp program does not call this directly; it calls
   --  Per_File_Action. Format_Vector is for calling from text editors and
   --  the like. Format_Vector is called from lalstub and partial-gnatpp.

   procedure Set_Partial_Gnatpp_Offset (Val : Natural);
   function Get_Partial_Gnatpp_Offset return Natural;
   --  These two accessors are used by partial_gnatpp to write/read
   --  global Partial_Gnatpp_Offset variable value. They are used exclusively
   --  for the partial formatting of the code.

private

   overriding procedure Init
     (Tool : in out Pp_Tool; Cmd : in out Command_Line);
   overriding procedure Per_File_Action
     (Tool : in out Pp_Tool;
      Cmd : Command_Line;
      File_Name : String;
      Input : String;
      BOM_Seen : Boolean;
      Unit : Analysis_Unit);
   overriding procedure Final (Tool : in out Pp_Tool; Cmd : Command_Line);
   overriding procedure Tool_Help (Tool : Pp_Tool);

   type Pp_Tool is new Tool_State with null record;

   --  For Debugging:

   procedure Dump
     (Tool : in out Pp_Tool;
      Message : String := "");

end Pp.Actions;
