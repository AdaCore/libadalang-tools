-- C83F01A.ADA

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
-- CHECK THAT INSIDE A PACKAGE BODY, AN ATTEMPT TO REFERENCE AN IDENTI-
--    FIER DECLARED IN THE CORRESPONDING PACKAGE SPECIFICATION
--    IS SUCCESSFUL, EVEN IF THE SAME IDENTIFIER IS DECLARED IN THE
--    ENVIRONMENT SURROUNDING THE PACKAGE BODY.

-- NESTED PACKAGE BODIES ARE TESTED IN C83F01B , C83F01C , C83F01D

--    RM    05 AUGUST 1980
--    JRK   13 NOV    1980

with Report;
procedure C83f01a is

   use Report;

   X1, X2 : Integer range 1 .. 23 := 17;

   type T1 is (A, B, C);

   Z : T1 := A;

begin

   Test
     ("C83F01A",
      "CHECK THAT INSIDE A PACKAGE BODY, " &
      "AN ATTEMPT TO REFERENCE AN IDENTIFIER " &
      "DECLARED IN THE CORRESPONDING PACKAGE SPECI" &
      "FICATION IS SUCCESSFUL EVEN IF THE SAME IDEN" &
      "TIFIER IS DECLARED IN THE ENVIRONMENT SURROUND" &
      "ING THE PACKAGE BODY");

   Comment ("NESTED PACKAGE BODIES ARE TESTED IN  C83F01B , -C , -D");

   declare

      package P is

         X1 : Boolean               := False;
         X2 : Integer range 1 .. 23 := 11;
         Y1 : Boolean               := True;
         Y2 : Integer               := 5;
         T1 : Integer               := 6;
         Z  : Integer               := 7;

      end P;

      Y1, Y2 : Integer := 13;

      package body P is
      begin

         X1 := X1 or Y1;
         Z  := Z + T1;
         Y2 := X2 * Y2;

         -- INCORRECT INTERPRETATIONS IN THE FIRST TWO
         --    ASSIGNMENTS  MANIFEST THEMSELVES AT
         --    COMPILE TIME AS TYPE ERRORS.

      end P;

   begin

      if X1 /= 17 or
        Z /= A or
        Y2 /= 13 or
        not P.X1 or
        P.Z /= 13 or
        P.Y2 /= 55
      then
         Failed ("INCORRECT ACCESSING");
      end if;

   end;

   Result;   --  POSS. ERROR DURING ELABORATION OF  P

end C83f01a;
