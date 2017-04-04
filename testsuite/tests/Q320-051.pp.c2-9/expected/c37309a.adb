-- C37309A.ADA

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
-- CHECK THAT IF A DISCRIMINANT HAS A STATIC SUBTYPE, AN OTHERS CHOICE CAN BE
-- OMITTED IF ALL VALUES IN THE SUBTYPE'S RANGE ARE COVERED IN A VARIANT PART.

-- ASL 7/10/81
-- SPS 10/25/82
-- SPS 7/17/83

with Report;
procedure C37309a is

   use Report;

begin
   Test
     ("C37309A",
      "OTHERS CHOICE CAN BE OMITTED IN VARIANT PART " &
      "IF ALL VALUES IN STATIC SUBTYPE RANGE OF DISCRIMINANT " &
      "ARE COVERED");

   declare
      subtype Statchar is Character range 'I' .. 'N';
      type Rec1 (Disc : Statchar := 'J') is record
         case Disc is
            when 'I' =>
               null;
            when 'J' =>
               null;
            when 'K' =>
               null;
            when 'L' =>
               null;
            when 'M' =>
               null;
            when 'N' =>
               null;
         end case;
      end record;

      R1 : Rec1;
   begin
      R1 := (Disc => 'N');
      if Equal (3, 3) then
         R1 := (Disc => 'K');
      end if;
      if R1.Disc /= 'K' then
         Failed ("ASSIGNMENT FAILED - 1");
      end if;

   exception
      when others =>
         Failed ("EXCEPTION RAISED");
   end;

   Result;

end C37309a;
