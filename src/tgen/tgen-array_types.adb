------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

package body TGen.Array_Types is

   use TGen.Types.SP;

   function Image (Self : Unconstrained_Array_Typ) return String is
      Res : Unbounded_String :=
        To_Unbounded_String (Typ (Self).Image & " : array (");
   begin
      for J in Self.Index_Types'Range loop
         if Self.Index_Types (J) /= SP.Null_Ref then
            Res := Res & Typ (Self.Index_Types (J).Get.Element.all).Image;
         else
            Res := Res & "null type ..";
         end if;
         if J /= Self.Index_Types'Last then
            Res := Res & " range <>, ";
         else
            Res := Res & " range <>) ";
         end if;
      end loop;
      Res := Res & "of " & Self.Component_Type.Get.Image;
      return To_String (Res);
   end Image;

   function Image (Self : Constrained_Array_Typ) return String is
      Res : Unbounded_String :=
        To_Unbounded_String (Typ (Self).Image & " : array (");
   begin
      for J in Self.Index_Types'Range loop
         if Self.Index_Types (J) /= SP.Null_Ref then
            Res := Res & Typ (Self.Index_Types (J).Get.Element.all).Image;
            if Self.Index_Constraints (J).Present
              and then Self.Index_Constraints (J)
                       .Discrete_Range.Low_Bound.Kind = Static
              and then Self.Index_Constraints (J)
                       .Discrete_Range.High_Bound.Kind = Static
            then
               Res :=
                 Res & " range "
                 & As_Discrete_Typ (Self.Index_Types (J)).Lit_Image
                     (Self.Index_Constraints (J).Discrete_Range.Low_Bound
                      .Int_Val)
                 & " .. "
                 & As_Discrete_Typ (Self.Index_Types (J)).Lit_Image
                     (Self.Index_Constraints (J).Discrete_Range.High_Bound
                      .Int_Val);
            end if;
         else
            Res := Res & "null type ..";
         end if;

         if J /= Self.Index_Types'Last then
            Res := Res & ", ";
         else
            Res := Res & ") ";
         end if;
      end loop;
      Res := Res & "of " & (if Self.Component_Type /= SP.Null_Ref
                            then Self.Component_Type.Get.Image
                            else "null type ..");
      return To_String (Res);
   end Image;

end TGen.Array_Types;
