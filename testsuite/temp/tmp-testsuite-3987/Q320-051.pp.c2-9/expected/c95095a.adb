-- C95095A.ADA

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
-- CHECK THAT OVERLOADED SUBPROGRAM AND ENTRY DECLARATIONS
-- ARE PERMITTED IN WHICH THERE IS A MINIMAL
-- DIFFERENCE BETWEEN THE DECLARATIONS.

--     (A) A FUNCTION AND AN ENTRY.

-- JWC 7/24/85

with Report; use Report;
procedure C95095a is

begin
   Test
     ("C95095A",
      "SUBPROGRAM/ENTRY OVERLOADING WITH " & "MINIMAL DIFFERENCES");

   --------------------------------------------------

   -- BOTH PARAMETERIZED AND PARAMETERLESS SUBPROGRAMS AND ENTRIES
   -- ARE TESTED.

   declare
      I, J, K : Integer         := 0;
      S       : String (1 .. 2) := "12";

      task T is
         entry E1 (I1, I2 : Integer);
         entry E2;
      end T;

      task body T is
      begin
         loop
            select
               accept E1 (I1, I2 : Integer) do
                  S (1) := 'A';
               end E1;
            or
               accept E2 do
                  S (1) := 'C';
               end E2;
            or
               terminate;
            end select;
         end loop;
      end T;

      function E1 (I1, I2 : Integer) return Integer is
      begin
         S (2) := 'B';
         return I1; -- RETURNED VALUE IS IRRELEVENT.
      end E1;

      function E2 return Integer is
      begin
         S (2) := 'D';
         return I; -- RETURNED VALUE IS IRRELEVENT.
      end E2;

   begin
      T.E1 (I, J);
      K := E1 (I, J);

      if S /= "AB" then
         Failed
           ("PARAMETERIZED OVERLOADED " &
            "SUBPROGRAM AND ENTRY " &
            "CAUSED CONFUSION");
      end if;

      S := "12";
      T.E2;
      K := E2;

      if S /= "CD" then
         Failed
           ("PARAMETERLESS OVERLOADED " &
            "SUBPROGRAM AND ENTRY " &
            "CAUSED CONFUSION");
      end if;
   end;

   --------------------------------------------------

   Result;
end C95095a;
