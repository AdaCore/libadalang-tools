------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

package body TGen.Subprograms is

   ---------------
   -- To_String --
   ---------------

   function To_String (Subp : Subprogram_Data) return String is
      Res : Unbounded_String;
      I : Natural := 0;
   begin
      Write_Line (Res, "function " & (+Subp.Name), I);
      I := I + 3;
      S_Write (Res, "(", I);

      --  Append the discriminants as parameters

      for Param_Cursor in Subp.Parameters_Data.Iterate loop
         declare
            use Parameters_Data_Vectors;

            Param_Data : constant Parameter_Data := Element (Param_Cursor);
            Param_Name : constant Unbounded_Text_Type := Param_Data.Name;
            Param_Type : constant Unbounded_Text_Type :=
              Param_Data.Type_Fully_Qualified_Name;
         begin
            S_Write (Res, (+Param_Name) & " : ", I);
            Write (Res, +Param_Type);
            if Next (Param_Cursor) = Parameters_Data_Vectors.No_Element then
               Write (Res, ")");
               Write
                 (Res,
                  " return "
                  & (+Subp.Return_Type_Fully_Qualified_Name));
               I := I - 3;
               New_Line (Res);
            else
               Write (Res, ";");
            end if;
         end;
      end loop;

      return +Res;
   end To_String;

end TGen.Subprograms;
