-- C95095B.ADA

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
-- CHECK THAT OVERLOADED ENTRY DECLARATIONS
-- ARE PERMITTED IN WHICH THERE IS A MINIMAL
-- DIFFERENCE BETWEEN THE DECLARATIONS.

--     (B) ONE ENTRY HAS ONE LESS PARAMETER THAN THE OTHER.

-- JWC 7/24/85
-- JRK 10/2/85

with Report; use Report;
procedure C95095b is

begin
   Test ("C95095B", "ENTRY OVERLOADING WITH " & "MINIMAL DIFFERENCES");

   --------------------------------------------------

   -- ONE ENTRY HAS ONE MORE PARAMETER
   -- THAN THE OTHER.  THIS IS TESTED IN THE
   -- CASE IN WHICH THAT PARAMETER HAS A DEFAULT
   -- VALUE, AND THE CASE IN WHICH IT DOES NOT.

   declare
      I, J : Integer         := 0;
      B    : Boolean         := True;
      S    : String (1 .. 2) := "12";

      task T is
         entry E1 (I1, I2 : Integer; B1 : in out Boolean);
         entry E1 (I1, I2 : Integer);
         entry E2 (B1 : in out Boolean; I1 : Integer := 0);
         entry E2 (B1 : in out Boolean);
      end T;

      task body T is
      begin
         loop
            select
               accept E1 (I1, I2 : Integer; B1 : in out Boolean) do
                  S (1) := 'A';
               end E1;
            or
               accept E1 (I1, I2 : Integer) do
                  S (2) := 'B';
               end E1;
            or
               accept E2 (B1 : in out Boolean; I1 : Integer := 0) do
                  S (1) := 'C';
               end E2;
            or
               accept E2 (B1 : in out Boolean) do
                  S (2) := 'D';
               end E2;
            or
               terminate;
            end select;
         end loop;
      end T;

   begin
      T.E1 (I, J, B);
      T.E1 (I, J);

      if S /= "AB" then
         Failed
           ("ENTRIES DIFFERING ONLY IN " &
            "NUMBER OF PARAMETERS (NO DEFAULTS) " &
            "CAUSED CONFUSION");
      end if;

      S := "12";
      T.E2 (B, I);
      -- NOTE THAT A CALL TO T.E2 WITH ONLY
      -- ONE PARAMETER IS AMBIGUOUS.

      if S /= "C2" then
         Failed
           ("ENTRIES DIFFERING ONLY IN " &
            "EXISTENCE OF ONE PARAMETER (WITH " &
            "DEFAULT) CAUSED CONFUSION");
      end if;
   end;

   --------------------------------------------------

   Result;
end C95095b;
