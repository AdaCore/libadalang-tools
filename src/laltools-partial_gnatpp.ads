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

with Utils.Char_Vectors;

package Laltools.Partial_GNATPP is
   use Langkit_Support.Slocs;
   use Libadalang.Analysis;
   use Laltools.Refactor;

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
     (Unit           :     Analysis_Unit;
      SL_Range       : Source_Location_Range;
      Start_Node     : out Ada_Node;
      End_Node       : out Ada_Node;
      Enclosing_Node : out Ada_Node;
      Input_Sel      : out Utils.Char_Vectors.Char_Vector);
   --  Retrieves the first and the last Ada node of a given selection range.
   --  These might be the same relevant node or different nodes depending on
   --  the initial text selection.
   --  The closest enclosing parent is also computed. It will be the start or
   --  end node when these are identical and the first common parent when
   --  these are different.
   --  Input_Sel will contain the selected region of the file to be rewritten.

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

end Laltools.Partial_GNATPP;
