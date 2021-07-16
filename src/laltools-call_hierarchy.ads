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
--  This package contains utilities for call hierarchy tools, for example,
--  subprograms to find all outgoing or incomming calls of a subprogram.

with GNATCOLL.Traces;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;

with Laltools.Common; use Laltools.Common;

package Laltools.Call_Hierarchy is

   procedure Find_Incoming_Calls
     (Definition         : Defining_Name'Class;
      Units              : Analysis_Unit_Array;
      Visit              : not null access procedure
        (Call_Identifier : Base_Id'Class;
         Kind            : Ref_Result_Kind;
         Cancel          : in out Boolean);
      Follow_Renamings   : Boolean := True;
      Imprecise_Fallback : Boolean := False)
     with Pre => Is_Subprogram (Definition.P_Basic_Decl)
                 or else Definition.P_Basic_Decl.Kind in
                   Ada_Generic_Subp_Instantiation_Range;
   --  For all Units, finds all incomming calls of the subprogram given by
   --  Definition and calls Visit on each call that was found. Exits early if
   --  Cancel if modified by Visit to True.
   --  If Follow_Renamings is True also includes calls that ultimately refer
   --  to Definition, by unwinding renaming clauses.
   --  If Imprecise_Fallback is True also includes calls that imprecisely refer
   --  to Definition.

   procedure Find_Outgoing_Calls
     (Definition : Defining_Name;
      Callback   : not null access procedure
        (Subp_Call : Ada_Node'Class);
      Trace      : GNATCOLL.Traces.Trace_Handle;
      Imprecise  : in out Boolean)
     with Pre => Is_Subprogram (Definition.P_Basic_Decl);
   --  Finds all outgoing calls of the subprogram given by Definition and
   --  calls Callback on each call that was found.
   --  If Imprecise_Fallback is True also includes calls that imprecisely refer
   --  to Definition.

end Laltools.Call_Hierarchy;
