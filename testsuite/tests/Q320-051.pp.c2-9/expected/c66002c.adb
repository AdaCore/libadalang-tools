-- C66002C.ADA

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

--     (C) ONE SUBPROGRAM HAS ONE LESS PARAMETER THAN THE OTHER.

-- CVP 5/4/81
-- JRK 5/8/81
-- NL 10/13/81

with Report;
procedure C66002c is

   use Report;

begin
   Test
     ("C66002C",
      "SUBPROGRAM OVERLOADING WITH " & "MINIMAL DIFFERENCES ALLOWED");

   --------------------------------------------------

   -- ONE PROCEDURE HAS ONE MORE PARAMETER THAN THE OTHER. THIS IS TESTED IN
   -- THE CASE IN WHICH THAT PARAMETER HAS A DEFAULT VALUE, AND THE CASE IN
   -- WHICH IT DOES NOT.

   declare
      I, J : Integer         := 0;
      B    : Boolean         := True;
      S    : String (1 .. 2) := "12";

      procedure P1 (I1, I2 : Integer; B1 : in out Boolean) is
      begin
         S (1) := 'A';
      end P1;

      procedure P1 (I1, I2 : Integer) is
      begin
         S (2) := 'B';
      end P1;

      procedure P2 (B1 : in out Boolean; I1 : Integer := 0) is
      begin
         S (1) := 'C';
      end P2;

      procedure P2 (B1 : in out Boolean) is
      begin
         S (2) := 'D';
      end P2;

   begin
      P1 (I, J, B);
      P1 (I, J);

      if S /= "AB" then
         Failed
           ("PROCEDURES DIFFERING ONLY IN " &
            "NUMBER OF PARAMETERS (NO DEFAULTS) " & "CAUSED CONFUSION");
      end if;

      S := "12";
      P2 (B, I);
      -- NOTE THAT A CALL TO P2 WITH ONLY ONE PARAMETER IS AMBIGUOUS.

      if S /= "C2" then
         Failed
           ("PROCEDURES DIFFERING ONLY IN " &
            "EXISTENCE OF ONE PARAMETER (WITH " & "DEFAULT) CAUSED CONFUSION");
      end if;
   end;

   --------------------------------------------------

   Result;

end C66002c;
