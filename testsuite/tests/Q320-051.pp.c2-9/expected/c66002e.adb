-- C66002E.ADA

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

--     (E) ONE SUBPROGRAM IS DECLARED IN AN OUTER DECLARATIVE
--         PART, THE OTHER IN AN INNER PART, AND THE PARAMETERS ARE
--         ORDERED DIFFERENTLY.

-- CVP 5/4/81
-- JRK 5/8/81
-- NL 10/13/81

with Report;
procedure C66002e is

   use Report;

begin
   Test
     ("C66002E",
      "SUBPROGRAM OVERLOADING WITH " & "MINIMAL DIFFERENCES ALLOWED");

   --------------------------------------------------

   -- ONE SUBPROGRAM IS DECLARED IN AN OUTER DECLARATIVE PART, THE OTHER IN AN
   -- INNER PART, AND THE PARAMETERS ARE ORDERED DIFFERENTLY.

   declare
      S : String (1 .. 2) := "12";

      procedure P (I1 : Integer; I2 : in out Integer; B1 : Boolean) is
      begin
         S (1) := 'A';
      end P;

   begin
      declare
         I : Integer := 0;

         procedure P (B1 : Boolean; I1 : Integer; I2 : in out Integer) is
         begin
            S (2) := 'B';
         end P;

      begin
         P (5, I, True);
         P (True, 5, I);
         -- NOTE THAT A CALL IN WHICH ALL ACTUAL PARAMETERS ARE
         -- NAMED_ASSOCIATIONS IS AMBIGUOUS.

         if S /= "AB" then
            Failed
              ("PROCEDURES IN " & "ENCLOSING-ENCLOSED SCOPES " &
               "DIFFERING ONLY IN PARAMETER " & "TYPE ORDER CAUSED CONFUSION");
         end if;
      end;
   end;

   --------------------------------------------------

   Result;

end C66002e;
