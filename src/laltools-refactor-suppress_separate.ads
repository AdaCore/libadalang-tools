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
---
---  This package contains refactoring tools that allow suppressing a
--   separate subprogram

package Laltools.Refactor.Suppress_Separate is

   function Is_Suppress_Separate_Available
     (Node            : Ada_Node;
      Target_Separate : out Basic_Decl)
      return Boolean;
   --  Checks if the Suppress Separate refactoring is available on Node.
   --  If so, sets 'Target_Separate' with the appropriate values to be used by
   --  the function 'Suppress_Separate' or to create a 'Separate_Suppressor'
   --  object
   --  Otherwise, sets 'Target_Separate' to 'No_Subunit';

   function Suppress_Separate
     (Target_Separate : Basic_Decl)
      return Refactoring_Edits
     with Pre => not Target_Separate.Is_Null;
   --  Returns all the needed edits to suppress a 'Subunit'.
   --  'Refactoring_Edits' will contain the file that needs to be deleted and
   --  the text edits needed in the separate's parent package body.

   type Separate_Suppressor is new Refactoring_Tool with private;

   overriding
   function Refactor
     (Self           : Separate_Suppressor;
      Analysis_Units : access function return Analysis_Unit_Array := null)
      return Refactoring_Edits;
   --  Runs the refactoring analysis and returns all the needed edits to
   --  suppress a 'Subunit'. The 'Refactoring_Edits' will contain the file that
   --  needs to be deleted and the text edits needed in the separate's parent
   --  package body.
   --  'Analysis_Units' parameter is not nedded for this 'Refactoring_Tool' and
   --  is therefore unused.

   function Create
     (Target_Separate : Basic_Decl)
      return Separate_Suppressor
     with Pre => not Target_Separate.Is_Null;
   --  Creates a Separate_Suppressor that will suppress 'Target_Separate'

private

   type Separate_Suppressor is new Refactoring_Tool with
      record
         Target_Separate : Basic_Decl;
      end record;

end Laltools.Refactor.Suppress_Separate;
