-- CDA201C.ADA

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
--     CONVERSION BETWEEN CONSTRAINED ARRAY AND RECORD TYPES.

-- HISTORY:
--     JET 09/12/88  CREATED ORIGINAL TEST.
--     DHH 05/17/89  CHANGED FROM '.DEP' TEST TO '.ADA' TEST.

with Report; use Report;
with Unchecked_Conversion;
procedure Cda201c is

   type Int is new Integer;

   type Arr is array (1 .. 2) of Integer;
   type Arr2 is array (Arr'Range) of Int;

   type Rec is record
      D : Integer;
      I : Integer;
   end record;

   type Rec2 is record
      D : Int;
      I : Int;
   end record;

   A : Arr2;
   R : Rec2;

   function Arr_Conv is new Unchecked_Conversion (Arr, Arr2);
   function Rec_Conv is new Unchecked_Conversion (Rec, Rec2);

begin
   Test
     ("CDA201C",
      "CHECK THAT UNCHECKED_CONVERSION CAN BE " &
      "INSTANTIATED FOR CONVERSION BETWEEN " &
      "CONSTRAINED ARRAY AND RECORD TYPES");

   A := Arr_Conv (Arr'(Arr'Range => Ident_Int (-1)));

   if A /= Arr2'(Arr'Range => -1) then
      Failed ("INCORRECT RESULT FROM ARRAY CONVERSION");
   end if;

   R := Rec_Conv (Rec'(D | I => Ident_Int (1)));

   if R /= Rec2'(D => 1, I => 1) then
      Failed ("INCORRECT RESULT FROM RECORD CONVERSION");
   end if;

   Result;
end Cda201c;
