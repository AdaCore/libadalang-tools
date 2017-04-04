-- C83F03B.ADA

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
-- CHECK THAT IF A PACKAGE BODY IS NESTED INSIDE ANOTHER PACKAGE BODY
--    THE INNER PACKAGE BODY CAN CONTAIN A LABEL IDENTIFIER IDENTICAL
--    TO A LABEL IDENTIFIER IN THE OUTER PACKAGE BODY, TO AN IDENTI-
--    FIER DECLARED IN THE OUTER PACKAGE BODY OR IN ITS SPECIFICATION,
--    OR TO A LABEL IDENTIFIER OR OTHER IDENTIFIER IN THE
--    ENVIRONMENT SURROUNDING THE OUTER PACKAGE BODY.

-- INTERACTIONS WITH SEPARATE COMPILATION ARE TESTED IN C83F03C ,
--    C83F03D .

--    RM    04 SEPTEMBER 1980

with Report;
procedure C83f03b is

   use Report;

   X1, X2 : Integer range 1 .. 23 := 17;

   type T1 is (A, B, C);

   Z : T1 := A;

   Flow_Index : Integer := 0;

begin

   Test
     ("C83F03B",
      "CHECK THAT IF A PACKAGE BODY IS NESTED" &
      " INSIDE ANOTHER PACKAGE BODY, THE INNER" &
      " PACKAGE BODY CAN CONTAIN A LABEL IDENTIFIER" &
      " IDENTICAL TO A LABEL IDENTIFIER IN THE OUTER" &
      " PACKAGE BODY, TO AN IDENTIFIER DECLARED IN" &
      " THE OUTER PACKAGE BODY OR IN ITS SPECIFICA" &
      "TION, OR TO A LABEL IDENTIFIER OR OTHER" &
      " IDENTIFIER IN THE ENVIRONMENT SURROUNDING" &
      " THE OUTER PACKAGE BODY");

   declare

      Y1, Y2 : Integer := 100;

      X2 : Integer := 100;

      procedure Bump is
      begin
         Flow_Index := Flow_Index + 1;
      end Bump;

      package Outer is

         Y3 : Integer := 100;

         type T3 is (D, E, F);

         package P is
            Aa : Boolean := False;
         end P;

      end Outer;

      package body Outer is

         Y4 : Integer := 200;

         type T4 is (G, H, I);

         package body P is
         begin

            goto X1;

            Bump;
            Bump;

            <<X1>>
            Bump;
            goto X2;
            Bump;
            <<T1>>
            Bump;
            goto Z;
            Bump;
            <<Y1>>
            Bump;
            goto Y2;
            Bump;
            <<Y2>>
            Bump;
            goto T1;
            Bump;
            <<X2>>
            Bump;
            goto Y1;
            Bump;
            <<Z>>
            Bump;
            goto T3;
            Bump;
            <<T3>>
            Bump;
            goto T4;
            Bump;
            <<LABEL_IN_OUTER>>
            Bump;
            goto Label_In_Main;
            Bump;
            <<Y3>>
            Bump;
            goto Y4;
            Bump;
            <<Y4>>
            Bump;
            goto Label_In_Outer;
            Bump;
            <<T4>>
            Bump;
            goto Y3;
            Bump;
            <<LABEL_IN_MAIN>>
            Bump;
            goto Ending;
            Bump;

            <<ENDING>>
            null;

         end P;

      begin

         <<LABEL_IN_OUTER>>
         null;

      end Outer;

   begin

      <<LABEL_IN_MAIN>>

      if Flow_Index /= 12 then
         Failed ("INCORRECT FLOW OF CONTROL");
      end if;

   end;

   Result;   --  POSS. ERROR DURING ELABORATION OF  P

end C83f03b;
