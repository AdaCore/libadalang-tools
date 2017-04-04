-- C95095D.ADA

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
-- CHECK THAT OVERLOADED SUBPROGRAM AND ENTRY DECLARATIONS ARE PERMITTED IN
-- WHICH THERE IS A MINIMAL DIFFERENCE BETWEEN THE DECLARATIONS.

--     (D) A SUBPROGRAM IS DECLARED IN AN OUTER DECLARATIVE
--         PART, AN ENTRY IS DECLARED IN A TASK, AND THE
--         PARAMETERS ARE ORDERED DIFFERENTLY.

-- JWC 7/24/85
-- JRK 10/2/85
-- PWN 09/11/94 REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Report; use Report;
with System; use System;
procedure C95095d is

begin
   Test
     ("C95095D",
      "SUBPROGRAM/ENTRY OVERLOADING WITH " & "MINIMAL DIFFERENCES");

   --------------------------------------------------

   -- A SUBPROGRAM IS DECLARED IN AN OUTER DECLARATIVE PART, AN ENTRY IS
   -- DECLARED IN A TASK, AND THE PARAMETERS ARE ORDERED DIFFERENTLY.

   declare
      S : String (1 .. 2) := "12";

      I : Integer := 0;

      procedure E (I1 : Integer; I2 : in out Integer; B1 : Boolean) is
      begin
         S (1) := 'A';
      end E;

      task T is
         entry E (B1 : Boolean; I1 : Integer; I2 : in out Integer);
      end T;

      task body T is
      begin
         E (5, I, True);          -- PROCEDURE CALL.
         accept E (B1 : Boolean; I1 : Integer; I2 : in out Integer) do
            S (2) := 'B';
         end E;
         E (True, 5, I);          -- ENTRY CALL; SELF-BLOCKING.
         -- NOTE THAT A CALL IN WHICH ALL ACTUAL PARAMETERS ARE
         -- NAMED_ASSOCIATIONS IS AMBIGUOUS.
         Failed ("TASK DID NOT BLOCK ITSELF");
      end T;

   begin

      T.E (True, 5, I);

      delay 10.0;
      abort T;

      if S /= "AB" then
         Failed
           ("PROCEDURES/ENTRIES " &
            "DIFFERING ONLY IN PARAMETER " &
            "TYPE ORDER CAUSED CONFUSION");
      end if;
   end;

   --------------------------------------------------

   Result;
end C95095d;
