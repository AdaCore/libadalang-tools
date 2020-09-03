------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with Libadalang.Analysis;

package Laltools.Call_Hierarchy is

   package LALAnalysis renames Libadalang.Analysis;

   procedure Find_Outgoing_Calls
     (Definition : LALAnalysis.Defining_Name;
      Callback   : not null access procedure
        (Subp_Call : LALAnalysis.Ada_Node'Class);
      Trace      : GNATCOLL.Traces.Trace_Handle;
      Imprecise  : in out Boolean)
     with Pre => Definition.P_Basic_Decl.P_Is_Subprogram;
   --  Finds all outgoing calls of the subprogram given by Definition and
   --  calls Callback on each call that was found.

end Laltools.Call_Hierarchy;
