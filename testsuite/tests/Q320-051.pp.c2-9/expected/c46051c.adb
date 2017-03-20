-- C46051C.ADA

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
--     CHECK THAT RECORD VALUES CAN BE CONVERTED IF THE OPERAND
--     AND TARGET TYPES ARE RELATED BY DERIVATION, EVEN IF THE OPERAND
--     AND TARGET TYPES HAVE DIFFERENT REPRESENTATIONS.

-- HISTORY:
--     JET 07/13/88  CREATED ORIGINAL TEST.
--     RJW 08/28/89  REMOVED APPLICABILITY CRITERIA AND CHANGED
--                   EXTENSION TO 'ADA'.

with Report; use Report;
with System;

procedure C46051c is

   Units_Per_Integer : constant :=
     (Integer'Size + System.Storage_Unit - 1) / System.Storage_Unit;

   type Arr is array (1 .. 2) of Integer;

   type Rec is record
      F1 : Integer;
      F2 : Integer;
      F3 : Integer;
   end record;

   type Rec1 is new Rec;
   for Rec1 use record
      F1 at                     0 range 0 .. Integer'Size - 1;
      F2 at 1 * Units_Per_Integer range 0 .. Integer'Size - 1;
      F3 at 3 * Units_Per_Integer range 0 .. Integer'Size - 1;
   end record;

   type Rec2 is new Rec;
   for Rec2 use record
      F1 at                     0 range 0 .. Integer'Size - 1;
      F2 at 2 * Units_Per_Integer range 0 .. Integer'Size - 1;
      F3 at 3 * Units_Per_Integer range 0 .. Integer'Size - 1;
   end record;

   type Rec3 is new Rec1;

   R  : Rec  := (Ident_Int (0), 1, 2);
   R1 : Rec1 := (Ident_Int (1), 2, 3);
   R2 : Rec2 := (Ident_Int (2), 3, 4);
   R3 : Rec3 := (Ident_Int (3), 4, 5);

begin
   Test
     ("C46051C",
      "CHECK THAT RECORD VALUES CAN BE " &
      "CONVERTED IF THE OPERAND AND TARGET TYPES " &
      "ARE RELATED BY DERIVATION, EVEN IF THE " &
      "OPERAND AND TARGET TYPES HAVE DIFFERENT " &
      "REPRESENTATIONS");

   if Rec1 (R) /= (0, 1, 2) then
      Failed ("INCORRECT CONVERSION OF 'REC1 (R)'");
   end if;

   if Rec (R1) /= (1, 2, 3) then
      Failed ("INCORRECT CONVERSION OF 'REC (R1)'");
   end if;

   if Rec1 (R2) /= (2, 3, 4) then
      Failed ("INCORRECT CONVERSION OF 'REC1 (R2)'");
   end if;

   if Rec2 (R3) /= (3, 4, 5) then
      Failed ("INCORRECT CONVERSION OF 'REC2 (R3)'");
   end if;

   if Rec (R) /= (0, 1, 2) then
      Failed ("INCORRECT CONVERSION OF 'REC (R)'");
   end if;

   if Rec2 (R1) /= (1, 2, 3) then
      Failed ("INCORRECT CONVERSION OF 'REC2 (R1)'");
   end if;

   if Rec3 (R2) /= (2, 3, 4) then
      Failed ("INCORRECT CONVERSION OF 'REC3 (R2)'");
   end if;

   if Rec (R3) /= (3, 4, 5) then
      Failed ("INCORRECT CONVERSION OF 'REC (R3)'");
   end if;

   Result;

exception
   when others =>
      Failed ("EXCEPTION RAISED DURING CONVERSION OF " & "RECORD TYPES");
      Result;
end C46051c;
