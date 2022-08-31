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

--  Common GNATpp partial selection utilities

with Langkit_Support.Slocs;
with Libadalang.Analysis;
with Laltools.Refactor;

with Pp.Command_Lines;
with Pp.Scanner;

with Utils.Char_Vectors;

package Laltools.Partial_GNATPP is
   use Langkit_Support.Slocs;
   use Libadalang.Analysis;
   use Laltools.Refactor;

   procedure Format_Selection
     (Main_Unit                : Analysis_Unit;
      Input_Selection_Range    : Source_Location_Range;
      Output                   : out Utils.Char_Vectors.Char_Vector;
      Output_Selection_Range   : out Source_Location_Range;
      PP_Messages              : out Pp.Scanner.Source_Message_Vector;
      Formatted_Node           : out Ada_Node;
      PP_Options               : Pp.Command_Lines.Cmd_Line;
      Force_Source_Line_Breaks : Boolean := True);
   --  This is the procedure to be called for the IDE integration with the
   --  Ada Language Server for the partial formatting of a text selection.
   --
   --  Starting from an initial selection given by Input_Selection_Range of the
   --  given Main_Unit, the procedure returns the formatted text related to the
   --  selection using the gnatpp engine for the formatting. The entry
   --  point for the gnatpp engine is Format_Vector from PP.Actions and is
   --  called during the formatting pass.
   --
   --  The Force_Source_Line_Breaks flag is set by default.
   --  This means that the initial source line breaks will be preserved during
   --  the formatting process and only the reformatted initially selected lines
   --  will be returned as values for Output and Output_Selection_Range.
   --  If this flag is not set then the enclosing parent node of the initial
   --  selection will be rewritten and the Output and Output_Selection_Range
   --  will contain the corresponding values related to this node. In this case
   --  the reformatted selection might be larger than the initial selection.
   --  The enclosing parent node is returned as value for Formatted_Node.
   --
   --  PP_Options contains the gnatpp switches to be used during the formatting
   --  process. PP_Messages contains the Error messages issued by gnatpp during
   --  the formatting process.

   type Partial_Select_Edits is
      record
         Unit : Analysis_Unit;
         Node : Ada_Node;
         Edit : Text_Edit;
      end record;
   --  Stores the selected region related information

   procedure Print (E : Partial_Select_Edits);
   --  Print an E in an human readable format to the standard output

   procedure Get_Selected_Region_Enclosing_Node
     (Unit             :     Analysis_Unit;
      SL_Range         : Source_Location_Range;
      Start_Node       : out Ada_Node;
      End_Node         : out Ada_Node;
      Enclosing_Node   : out Ada_Node;
      Input_Sel        : out Utils.Char_Vectors.Char_Vector;
      Output_Sel_Range : out Source_Location_Range);
   --  Retrieves the first and the last Ada node of a given selection range.
   --  These might be the same relevant node or different nodes depending on
   --  the initial text selection.
   --  The closest enclosing parent is also computed. It will be the start or
   --  end node when these are identical and the first common parent when
   --  these are different.
   --  Input_Sel will contain the selected region of the file to be rewritten.
   --  Output_Sel_Range contains the Source_Location_Range to be rewritten.

   function Get_Previous_Sibling (Node : Ada_Node) return Ada_Node;
   --  Returns the node's previous sibling or No_Ada_Node if no sibling found

   function Get_Next_Sibling (Node : Ada_Node) return Ada_Node;
   --  Returns the node's next sibling or No_Ada_Node if no sibling found

   function Get_Starting_Offset
     (Node                   : Ada_Node;
      PP_Indent              : Natural;
      PP_Indent_Continuation : Natural) return Natural;
   --  Returns the starting offset that needs to be used for the selected Node
   --  formatting

   procedure Filter_Initially_Selected_Lines_From_Output
     (Unit              : Analysis_Unit;
      Initial_SL_Range  : Source_Location_Range;
      Output            : Utils.Char_Vectors.Char_Vector;
      Output_SL_Range   : Source_Location_Range;
      New_Output        : out Utils.Char_Vectors.Char_Vector;
      New_SL_Range      : out Source_Location_Range);
   --  Retrieves the initial selected line(s) from the Output and returns
   --  the related Char_Vector with the associated new selection range.
   --  This will be used only for the case when the initial sourece line breaks
   --  are preserved.
   --  The Initial_SL_Range contains the initial source location range selected
   --  in the source file related to the given Unit.
   --  The Enclosing_Node is the enclosing parent that was reformatted.
   --  Input_Sel will contain the initial text selection of the enclosing node.
   --  Output and Output_SL_Range contains the results of Format_Vector.
   --  New_Output and New_SL_Range will contain the filtered lines of the
   --  reformatted selection.

end Laltools.Partial_GNATPP;
