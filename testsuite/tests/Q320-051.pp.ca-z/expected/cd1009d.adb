-- CD1009D.ADA

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
--     CHECK THAT A 'SIZE CLAUSE MAY BE GIVEN IN THE VISIBLE
--     OR PRIVATE PART OF A PACKAGE FOR A FIXED POINT TYPE DECLARED IN
--     THE VISIBLE PART OF THE SAME PACKAGE.

-- HISTORY:
--     PWB 03/25/89  MODIFIED METHOD OF CHECKING OBJECT SIZE AGAINST
--                   TYPE SIZE; CHANGED EXTENSION FROM '.ADA' TO '.DEP'.
--     VCL 10/07/87  CREATED ORIGINAL TEST.

with Report; use Report;
procedure Cd1009d is
begin
   Test
     ("CD1009D",
      "A 'SIZE CLAUSE MAY BE GIVEN IN THE VISIBLE " &
      "OR PRIVATE PART OF A PACKAGE FOR A " &
      "FIXED POINT TYPE DECLARED IN THE VISIBLE " &
      "PART OF THE SAME PACKAGE");
   declare
      package Pack is
         type Specified is delta 2.0**(-4) range 0.0 .. 10.0;

         Specified_Size : constant := Specified'Size;

         type Check_Type_1 is delta 2.0**(-1) range 0.0 .. 1.0;
         for Check_Type_1'Size use Specified_Size;

         type Check_Type_2 is delta 2.0**(-1) range 0.0 .. 1.0;
      private
         for Check_Type_2'Size use Specified_Size;
      end Pack;

      use Pack;

      X : Check_Type_1 := 0.5;
      Y : Check_Type_2 := 0.5;

   begin
      if Check_Type_1'Size /= Specified_Size then
         Failed ("CHECK_TYPE_1'SIZE IS INCORRECT");
      end if;

      if Check_Type_2'Size /= Specified_Size then
         Failed ("CHECK_TYPE_2'SIZE IS INCORRECT");
      end if;

      if X'Size < Specified_Size then
         Failed
           ("OBJECT SIZE IS TOO SMALL -- " & "VALUE IS" &
            Integer'Image (Integer (X)));
      end if;

      if Y'Size < Specified_Size then
         Failed
           ("OBJECT SIZE IS TOO SMALL -- " & "VALUE IS" &
            Integer'Image (Integer (Y)));
      end if;

   end;

   Result;
end Cd1009d;
