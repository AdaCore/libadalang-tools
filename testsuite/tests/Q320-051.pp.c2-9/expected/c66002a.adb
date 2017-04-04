-- C66002A.ADA

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
-- CHECK THAT OVERLOADED SUBPROGRAM DECLARATIONS ARE PERMITTED IN WHICH THERE
-- IS A MINIMAL DIFFERENCE BETWEEN THE DECLARATIONS.

--     (A) ONE SUBPROGRAM IS A FUNCTION; THE OTHER IS A PROCEDURE.

-- CVP 5/4/81
-- JRK 5/8/81
-- NL 10/13/81
-- SPS 11/2/82

with Report;
procedure C66002a is

   use Report;

begin
   Test
     ("C66002A",
      "SUBPROGRAM OVERLOADING WITH " & "MINIMAL DIFFERENCES ALLOWED");

   --------------------------------------------------

   -- ONE SUBPROGRAM IS A PROCEDURE; THE OTHER IS A FUNCTION. BOTH
   -- PARAMETERIZED AND PARAMETERLESS SUBPROGRAMS ARE TESTED.

   declare
      I, J, K : Integer         := 0;
      S       : String (1 .. 2) := "12";

      procedure P1 (I1, I2 : Integer) is
      begin
         S (1) := 'A';
      end P1;

      function P1 (I1, I2 : Integer) return Integer is
      begin
         S (2) := 'B';
         return I1; -- RETURNED VALUE IS IRRELEVENT.
      end P1;

      procedure P2 is
      begin
         S (1) := 'C';
      end P2;

      function P2 return Integer is
      begin
         S (2) := 'D';
         return I; -- RETURNED VALUE IS IRRELEVENT.
      end P2;

   begin
      P1 (I, J);
      K := P1 (I, J);

      if S /= "AB" then
         Failed
           ("PARAMETERIZED OVERLOADED " &
            "SUBPROGRAMS, ONE A PROCEDURE AND " &
            "THE OTHER A FUNCTION, CAUSED " &
            "CONFUSION");
      end if;

      S := "12";
      P2;
      K := P2;

      if S /= "CD" then
         Failed
           ("PARAMETERLESS OVERLOADED " &
            "SUBPROGRAMS, ONE A PROCEDURE AND " &
            "THE OTHER A FUNCTION, CAUSED " &
            "CONFUSION");
      end if;
   end;

   --------------------------------------------------

   Result;

end C66002a;
