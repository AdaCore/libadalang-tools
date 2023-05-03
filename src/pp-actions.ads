------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2021-2023, AdaCore                      --
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

with Pp.Scanner;

with Utils.Char_Vectors; use Utils.Char_Vectors;
with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Tools; use Utils.Tools;

use Utils.Char_Vectors.Char_Vectors;

package Pp.Actions is

   type Pp_Tool is new Tool_State with private;

   procedure Format_Vector
     (Cmd                 : Command_Line;
      Input               : Char_Vector;
      Node                : Ada_Node;
      Output              : out Char_Vector;
      Messages            : out Pp.Scanner.Source_Message_Vector;
      First_Line_Offset   : Natural := 0;
      Initial_Indentation : Natural := 0;
      Partial_GNATPP      : Boolean := False;
      Start_Child_Index   : Natural := 0;
      End_Child_Index     : Natural := 0)
     with Pre => Pp.Scanner.Source_Message_Vectors.Is_Empty (Messages);
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
   --     First_Line_Offset -- Apply an offset to the line length of the
   --     first line. This is needed to partially format nodes that do not
   --     start at the beggining of a line. For instance, in a Subp_Spec
   --     preceded by the "overriding" keyword (which is a sibling node),
   --     there might not be a LB between the two, therefore, the first line
   --     of the formatted must have an additional offset of 11 characters (10
   --     for the overriding keyword plus 1 for the blank).
   --
   --     Initial_Indentation -- Apply an extra indentation to the whole
   --     formatted node.
   --
   --     Partial_GNATPP -- Boolean parameterset when Format_Vector is called
   --     in a context of code snippet reformatting (from partial-gnatpp).
   --
   --     Start_Child_Index -- The index of the first node to format if
   --     Partial_GNATPP is true and if we're formatting a list.
   --
   --     End_Child_Index -- The index of the last node to format if
   --     Partial_GNATPP is true and if we're formatting a list.
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

   procedure Clear_Template_Tables;
   --  Clear all gnatpp's predefined template tables.
   --  Should be called everytime gnatpp's command line changes.

private

   overriding
   procedure Init
     (Tool : in out Pp_Tool;
      Cmd  : in out Command_Line);

   overriding
   procedure Per_File_Action
     (Tool      : in out Pp_Tool;
      Cmd       : Command_Line;
      File_Name : String;
      Input     : String;
      BOM_Seen  : Boolean;
      Unit      : Analysis_Unit);

   overriding
   procedure Final
     (Tool : in out Pp_Tool;
      Cmd  : Command_Line);

   overriding
   procedure Tool_Help (Tool : Pp_Tool);

   type Pp_Tool is new Tool_State with null record;

   --  For Debugging:

   procedure Dump
     (Tool    : in out Pp_Tool;
      Message : String := "");

end Pp.Actions;
