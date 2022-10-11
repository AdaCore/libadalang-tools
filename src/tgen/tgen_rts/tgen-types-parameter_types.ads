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

package TGen.Types.Parameter_Types is

   --  A convenient way to represent parameters as type values. The name of the
   --  type will be the parameter fully qualified name (which is the function
   --  FQN + the parameter name).

   type Parameter_Mode_Type is (In_Mode, In_Out_Mode, Out_Mode);

   type Parameter_Typ is new Typ with record
      Parameter_Type : SP.Ref;
      Parameter_Mode : Parameter_Mode_Type;
   end record;

   function Supports_Static_Gen (Self : Parameter_Typ) return Boolean is
     (True);
   --  Wether values for this Typ can be statically generated

   function Image (Self : Parameter_Typ) return String is
      (Typ (Self).Image & " : " & SP.Get (Self.Parameter_Type).Image);

   function Package_Name (Self : Parameter_Typ) return Ada_Qualified_Name;

   function Kind (Self : Parameter_Typ) return Typ_Kind is (Parameter_Kind);

   function As_Parameter_Typ (Self : SP.Ref)
     return Parameter_Typ'Class is
     (Parameter_Typ'Class (Self.Unchecked_Get.all)) with
     Pre => (not SP.Is_Null (Self))
            and then (Self.Get.Kind in Parameter_Kind);
   pragma Inline (As_Parameter_Typ);

end TGen.Types.Parameter_Types;
