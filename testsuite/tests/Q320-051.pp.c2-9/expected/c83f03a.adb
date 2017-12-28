-- C83F03A.ADA

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
-- CHECK THAT INSIDE A PACKAGE BODY AN ATTEMPT TO PLACE AND REFERENCE
--    A LABEL IS SUCCESSFUL EVEN IF ITS IDENTIFIER IS DECLARED IN THE
--    ENVIRONMENT SURROUNDING THE PACKAGE BODY.

-- NESTED PACKAGE BODIES ARE TESTED IN C83F03B , C83F03C , C83F03D

--    RM    03 SEPTEMBER 1980

with Report;
procedure C83f03a is

   use Report;

   X1, X2 : Integer range 1 .. 23 := 17;

   type T1 is (A, B, C);

   Z : T1 := A;

   Flow_Index : Integer := 0;

begin

   Test
     ("C83F03A",
      "CHECK THAT INSIDE A PACKAGE BODY " &
      " AN ATTEMPT TO PLACE AND REFERENCE A LABEL" &
      " IS SUCCESSFUL EVEN IF ITS IDEN" &
      "TIFIER IS DECLARED IN THE ENVIRONMENT SURROUND" &
      "ING THE PACKAGE BODY");

   declare

      Y1, Y2 : Integer := 13;

      procedure Bump is
      begin
         Flow_Index := Flow_Index + 1;
      end Bump;

      package P is

         Aa : Boolean := False;

      end P;

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
         goto Ending;
         Bump;

         <<Ending>>
         null;

      end P;

   begin

      if Flow_Index /= 6 then
         Failed ("INCORRECT FLOW OF CONTROL");
      end if;

   end;

   Result;   --  POSS. ERROR DURING ELABORATION OF  P

end C83f03a;
