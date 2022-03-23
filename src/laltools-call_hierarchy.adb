------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
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

package body Laltools.Call_Hierarchy is

   ---------------------------
   --  Find_Incomming_Calls --
   ---------------------------

   procedure Find_Incoming_Calls
     (Definition         : Defining_Name'Class;
      Units              : Analysis_Unit_Array;
      Visit              : not null access procedure
        (Call_Identifier : Base_Id'Class;
         Kind            : Ref_Result_Kind;
         Cancel          : in out Boolean);
      Follow_Renamings   : Boolean := True;
      Imprecise_Fallback : Boolean := False)
   is
      Cancel : Boolean := False;

   begin
      for Reference of
        Definition.P_Find_All_Calls
          (Units, Follow_Renamings, Imprecise_Fallback)
      loop
         Visit (Ref (Reference), Kind (Reference), Cancel);

         exit when Cancel;
      end loop;
   end Find_Incoming_Calls;

   -------------------------
   -- Find_Outgoing_Calls --
   -------------------------

   procedure Find_Outgoing_Calls
     (Definition : Defining_Name;
      Callback   : not null access procedure
        (Subp_Call : Ada_Node'Class);
      Trace      : GNATCOLL.Traces.Trace_Handle;
      Imprecise  : in out Boolean)
   is

      function Process_Body_Children (N : Ada_Node'Class)
                                      return Visit_Status;
      --  Check if N is a subprogram call and if so call callback.

      ----------------------------
      -- Process_Body_Childreen --
      ----------------------------

      function Process_Body_Children (N : Ada_Node'Class)
                                      return Visit_Status is
      begin
         --  Do not consider calls made by nested subprograms, expression
         --  functions or tasks.

         if N.Kind in
           Ada_Subp_Body
             | Ada_Subp_Spec
               | Ada_Expr_Function
                 | Ada_Task_Body
                   | Ada_Single_Task_Decl
                     | Ada_Task_Type_Decl
         then
            return Over;
         end if;

         if Is_Call (N, Trace, Imprecise) then
            Callback (N);
         end if;
         return Into;
      end Process_Body_Children;

      Bodies : constant Bodies_List.List :=
        List_Bodies_Of (Definition, Trace, Imprecise);
   begin
      --  Iterate through all the bodies, and for each, iterate
      --  through all the childreen looking for function calls.

      for B of Bodies loop
         for C of B.P_Basic_Decl.Children when not C.Is_Null loop
            C.Traverse (Process_Body_Children'Access);
         end loop;
      end loop;
   end Find_Outgoing_Calls;

end Laltools.Call_Hierarchy;
