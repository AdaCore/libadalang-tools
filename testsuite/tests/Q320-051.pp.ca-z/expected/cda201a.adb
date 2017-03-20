-- CDA201A.ADA

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
-- OBJECTIVE:
--     CHECK THAT UNCHECKED_CONVERSION CAN BE INSTANTIATED FOR
--     CONVERSION BETWEEN INTEGER AND BOOLEAN ARRAY TYPES.

-- HISTORY:
--     JET 09/12/88  CREATED ORIGINAL TEST.
--     DHH 05/17/89  CHANGED FROM '.DEP' TEST TO '.ADA' TEST.

with Report; use Report;
with Unchecked_Conversion;
procedure Cda201a is

   type Bool_Arr is array (1 .. Integer'Size) of Boolean;
   pragma Pack (Bool_Arr);

   I : Integer;
   B : Bool_Arr;

   function Int_To_Bool is new Unchecked_Conversion (Integer, Bool_Arr);

   function Bool_To_Int is new Unchecked_Conversion (Bool_Arr, Integer);

begin
   Test
     ("CDA201A",
      "CHECK THAT UNCHECKED_CONVERSION CAN BE " &
      "INSTANTIATED FOR CONVERSION BETWEEN " &
      "INTEGER AND BOOLEAN ARRAY TYPES");

   I := Bool_To_Int ((1 .. Integer'Size => Ident_Bool (True)));

   if Int_To_Bool (Ident_Int (I)) /= (1 .. Integer'Size => True) then
      Failed ("INCORRECT RESULT FROM ARRAY-INTEGER-ARRAY");
   end if;

   B := Int_To_Bool (Ident_Int (-1));

   for J in B'Range loop
      B (J) := Ident_Bool (B (J));
   end loop;

   if Bool_To_Int (B) /= -1 then
      Failed ("INCORRECT RESULT FROM INTEGER-ARRAY-INTEGER");
   end if;

   Result;
end Cda201a;
