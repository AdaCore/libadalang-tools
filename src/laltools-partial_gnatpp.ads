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

--  Common GNATPP partial selection utilities

with Ada.Strings.Unbounded;

with Langkit_Support.Slocs;

with Libadalang.Analysis;

with Pp.Command_Lines;
with Pp.Scanner;

with Utils.Char_Vectors;

package Laltools.Partial_GNATPP is

   use Langkit_Support.Slocs;
   use Libadalang.Analysis;

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

   type Text_Edit is
      record
         Location : Source_Location_Range;
         Text     : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Partial_Formatting_Edit is
      record
         Edit           : Text_Edit;
         Formatted_Node : Ada_Node;
         Diagnostics    : Pp.Scanner.Source_Message_Vector;
      end record;

   function Image (Edit : Partial_Formatting_Edit) return String;

   function Format_Selection
     (Unit                     : Analysis_Unit;
      Input_Selection_Range    : Source_Location_Range;
      PP_Options               : Pp.Command_Lines.Cmd_Line)
      return Partial_Formatting_Edit;
   --  This is the procedure to be called for the IDE integration with the
   --  Ada Language Server for the partial formatting of a text selection.
   --
   --  Starting from an initial selection given by Input_Selection_Range of the
   --  given Unit, the procedure returns the Partial_Formatting_Edit related to
   --  the selection using the gnatpp engine for the formatting. The entry
   --  point for the gnatpp engine is Format_Vector from PP.Actions and is
   --  called during the formatting pass.
   --
   --  PP_Options contains the gnatpp switches to be used during the formatting
   --  process. PP_Messages contains the Error messages issued by gnatpp during
   --  the formatting process.

   --  If Force_Source_Line_Breaks is True, the initial source line breaks will
   --  be preserved during the formatting process and only the reformatted
   --  initially selected lines will be returned in the
   --  Partial_Formatting_Edit.Edit.
   --  If this flag is not set then the enclosing parent node of the initial
   --  selection will be rewritten and the Partial_Formatting_Edit
   --  will contain the corresponding values related to this node. In this case
   --  the reformatted selection might be larger than the initial selection.

end Laltools.Partial_GNATPP;
