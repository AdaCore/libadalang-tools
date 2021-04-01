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

procedure Utils.Var_Length_Ints.Test is

   type My_Int is new Positive;

   package My_Int_Encodings is new Encodings (My_Int);
   use My_Int_Encodings;

   V : Octet_Vector;

   subtype Test_Cases is My_Int with
     Predicate => Test_Cases in 1 .. 1000
                             | 16_000 .. 17_000
                             | 2_097_100 .. 2_097_200
                             | 268_435_400 .. 268_435_500
                             | My_Int'Last - 100 .. My_Int'Last;

begin
   for X in Test_Cases loop
      Encode (V, X);
   end loop;

   declare
      Index : Octet_Index := 1;
      Actual : My_Int;
   begin
      for Expected in Test_Cases loop
         Actual := Decode (V, Index);
         if Actual /= Expected then
            raise Program_Error;
         end if;
         Next (V, Index);
      end loop;

      if Index /= Last_Index (V) + 1 then
         raise Program_Error;
      end if;

      for Expected in reverse Test_Cases loop
         Prev (V, Index);
         Actual := Decode (V, Index);
         if Actual /= Expected then
            raise Program_Error;
         end if;
      end loop;

      if Index /= 1 then
         raise Program_Error;
      end if;
   end;
end Utils.Var_Length_Ints.Test;
