------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
--                                                                          --
-- TGen  is  free software; you can redistribute it and/or modify it  under --
-- under  terms of  the  GNU General  Public License  as  published by  the --
-- Free  Software  Foundation;  either version 3, or  (at your option)  any --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Libadalang.Analysis; use Libadalang.Analysis;

with TGen.Types;              use TGen.Types;
with TGen.Types.Record_Types; use TGen.Types.Record_Types;

package TGen.Wrappers is

   type Subp_Information is record
      T : TGen.Types.SP.Ref;
      --  Specification of the subprogram (pointer to a Function_Typ)

      Pre : Unbounded_String;
      --  Precondition string of the subprogram
   end record;

   procedure Generate_Wrapper_For_Subprogram
     (F_Spec, F_Body     : File_Type;
      Subprogram         : Function_Typ;
      Precond            : Ada_Node;
      Templates_Root_Dir : String);
   --  Generate a wrapper function for the given subprogram. A wrapper wraps
   --  the call to the original function by checking that the precondition
   --  holds before calling it.
   --
   --  To check that the precondition holds, we split the evaluation of the
   --  precondition and turn it into its disjunctive normal form (DNF). Note
   --  that we limit the splitting to the overall decision, meaning that if
   --  some part of the decision are nested in a declare expression, they
   --  won't be splitted adequately. This is fine for now.
   --
   --  procedure Foo (I, J : Integer) with Pre => I > 0 and then J > 0;
   --
   --  becomes
   --
   --  procedure Foo_Wrapper (I, J : Integer) is
   --  begin
   --     if I > 0 and then J > 0 then
   --        Foo (I, J);
   --     else
   --        raise Precondition_Error;
   --     end if;
   --  end Foo_Wrapper;
   --
   --  More generally, assuming that the precondition is expressed in its DNF
   --  as:
   --
   --  (l_11 ^ l_12 ^ ... ^ l_1j) v .. v (l_i1 ^ l_i2 ^ ... ^ l_ij')
   --
   --  The wrapper will look like:
   --
   --  procedure Subp_Wrapper (l_11, l12 ... l_1j ... l_ij' : Boolean) is
   --  begin
   --     if l_11 and then l_12 and then ... and then l_1j then
   --        Subp (l_11, l12 ... l_1j ... l_ij);
   --     ...
   --     elsif l_i1 and then l_i2 and then ... and then l_ij' then
   --        Subp (l_11, l12 ... l_1j ... l_ij');
   --     else
   --        raise Precondition_Error;
   --     end if;
   --  end Subp_Wrapper;
   --
   --  TODO???: we could add globals specified by the Globals aspects as
   --  additional inputs to the subprogram wrapper to fuzz their value as well.
   --  Note: this would also require drastic changes in the value generation as
   --  (as we would need to generate values for them).

   Source_Package_Renaming : constant String := "TGen_Original_Package";
   --  Package name to be used to rename the original package when generating
   --  wrappers to avoid shadowing. This happens when a subprogram has
   --  parameters with the same name as the package in which it is declared.
   --
   --  For instance, given the following package declaration:
   --
   --  package Foo is
   --     procedure Bar (Foo : Integer) with
   --       Pre => True;
   --  end Foo;
   --
   --  The generated wrapper for Bar will look like:
   --
   --  procedure Bar (Foo : standard.Integer) is
   --  begin
   --     if not True then
   --        raise TGen.Precondition_Error;
   --     end if;
   --     Foo.Bar (Foo);
   --  end Bar;
   --
   --  In the body above, Foo designates the parameter, so Foo.Bar is undefined
   --  and the wrapper does not compile.
   --
   --  By introducing a package renaming, the wrapper package now looks like
   --
   --  package TGen_Original_Package renames Foo;
   --
   --  procedure Bar (Foo : standard.Integer) is
   --  begin
   --     --  ...
   --     TGen_Original_Package.Bar (Foo);
   --  end Bar;
   --
   --  This solves the masking issue.

end TGen.Wrappers;
