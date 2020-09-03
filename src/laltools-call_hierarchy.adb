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

with Laltools.Common;

with Libadalang.Common;

package body Laltools.Call_Hierarchy is

   package LALCommon renames Libadalang.Common;

   -------------------------
   -- Find_Outgoing_Calls --
   -------------------------

   procedure Find_Outgoing_Calls
     (Definition : LALAnalysis.Defining_Name;
      Callback   : not null access procedure
        (Subp_Call : LALAnalysis.Ada_Node'Class);
      Trace      : GNATCOLL.Traces.Trace_Handle;
      Imprecise  : in out Boolean)
   is

      function Process_Body_Children (N : LALAnalysis.Ada_Node'Class)
                                      return LALCommon.Visit_Status;
      --  Check if N is a subprogram call and if so call callback.

      ----------------------------
      -- Process_Body_Childreen --
      ----------------------------

      function Process_Body_Children (N : LALAnalysis.Ada_Node'Class)
                                      return LALCommon.Visit_Status is
      begin
         --  Do not consider calls made by nested subprograms, expression
         --  functions or tasks.

         if N.Kind in
           LALCommon.Ada_Subp_Body
             | LALCommon.Ada_Subp_Spec
               | LALCommon.Ada_Expr_Function
                 | LALCommon.Ada_Task_Body
                   | LALCommon.Ada_Single_Task_Decl
                     | LALCommon.Ada_Task_Type_Decl
         then
            return LALCommon.Over;
         end if;

         if Laltools.Common.Is_Call (N, Trace, Imprecise) then
            Callback (N);
         end if;
         return LALCommon.Into;
      end Process_Body_Children;

      Bodies : constant Laltools.Common.Bodies_List.List :=
        Laltools.Common.List_Bodies_Of (Definition, Trace, Imprecise);
   begin
      --  Iterate through all the bodies, and for each, iterate
      --  through all the childreen looking for function calls.

      for B of Bodies loop
         for C of B.P_Basic_Decl.Children loop
            C.Traverse (Process_Body_Children'Access);
         end loop;
      end loop;
   end Find_Outgoing_Calls;

end Laltools.Call_Hierarchy;
