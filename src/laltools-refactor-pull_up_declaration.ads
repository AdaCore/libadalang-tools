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
--  This package contains refactoring tools that allow pulling up
--  declarations to an upper level declarative part.
--  Currently, there is one limitation: declarations of nested packages
--  cannot be pulled up.

package Laltools.Refactor.Pull_Up_Declaration is

   function Is_Pull_Up_Declaration_Available
     (Unit      : Analysis_Unit;
      Node_SLOC : Source_Location)
      return Boolean;
   --  Checks if Unit and Declaration_SLOC represent a declaration that can be
   --  extracted. If so, Unit and Declaration_SLOC can be used on the
   --  Declaration_Extractor constructor Create_Declaration_Extractor.

   type Declaration_Extractor is new Refactoring_Tool with private;

   function Create_Declaration_Pull_Upper
     (Unit                     : Analysis_Unit;
      Declaration_SLOC         : Source_Location;
      Indentation              : Natural := 3;
      Only_Dependencies        : Boolean := False;
      Try_Subp_Insertion_Point : Boolean := False)
      return Declaration_Extractor
     with Pre => Is_Pull_Up_Declaration_Available (Unit, Declaration_SLOC);
   --  Declaration_Extractor constructor.
   --  Declaration_SLOC must be the SLOC of the declaration Name'Class node
   --  that will be extracted. Use Is_Pull_Up_Declaration_Available to check
   --  if a declaration can be extracted from Unit and Declaration_SLOC.

   overriding
   function Refactor
     (Self           : Declaration_Extractor;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Extracts the declaration identified by the Unit and Declaration_SLOC
   --  passed to the Declaration_Extractor constructor
   --  Create_Declaration_Extractor.

private

   type Declaration_Extractor is new Refactoring_Tool with
      record
         Declaration              : Basic_Decl;
         Indentation              : Natural := 3;
         Only_Dependencies        : Boolean := False;
         Try_Subp_Insertion_Point : Boolean := False;
      end record
     with Dynamic_Predicate => not Declaration.Is_Null;

end Laltools.Refactor.Pull_Up_Declaration;
