-- CD1009O.ADA

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
--     CHECK THAT A 'SIZE' CLAUSE MAY BE GIVEN IN THE PRIVATE PART
--     OF A PACKAGE FOR AN INCOMPLETE TYPE, WHOSE FULL DECLARATION
--     IS AN INTEGER TYPE, DECLARED IN THE VISIBLE PART OF THE SAME
--     PACKAGE.

-- HISTORY:
--     PWB 03/25/89  MODIFIED METHOD OF CHECKING OBJECT SIZE AGAINST
--                   TYPE SIZE; CHANGED EXTENSION FROM '.ADA' TO '.DEP'.
--     VCL  10/08/87  CREATED ORIGINAL TEST.

with Report; use Report;
procedure Cd1009o is
begin
   Test
     ("CD1009O",
      "A 'SIZE' CLAUSE MAY BE GIVEN IN THE PRIVATE " &
      "PART OF A PACKAGE FOR AN INCOMPLETE TYPE, " &
      "WHOSE FULL DECLARATION IS AN INTEGER " &
      "TYPE, DECLARED IN THE VISIBLE PART OF THE " &
      "SAME PACKAGE");
   declare
      package Pack is
         Specified_Size : constant := Integer'Size / 2;

         type Check_Type_1;
         type Acc is access Check_Type_1;

         type Check_Type_1 is range 0 .. 7;

      private
         for Check_Type_1'Size use Specified_Size;
      end Pack;

      use Pack;

      X : Check_Type_1 := Check_Type_1 (Ident_Int (1));

   begin
      if Check_Type_1'Size /= Specified_Size then
         Failed ("CHECK_TYPE_1'SIZE IS INCORRECT");
      end if;

      if X'Size < Specified_Size then
         Failed
           ("OBJECT SIZE TOO SMALL -- CHECK_TYPE_1.  " &
            "VALUE IS" &
            Check_Type_1'Image (X));
      end if;

   end;

   Result;
end Cd1009o;
