-- C66002G.ADA

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

--     (G) THE RESULT TYPE OF TWO FUNCTION DECLARATIONS IS DIFFERENT.

-- CVP 5/4/81
-- JRK 5/8/81
-- NL 10/13/81
-- SPS 10/26/82

with Report;
procedure C66002g is

   use Report;

begin
   Test
     ("C66002G",
      "SUBPROGRAM OVERLOADING WITH " & "MINIMAL DIFFERENCES ALLOWED");

   --------------------------------------------------

   -- THE RESULT TYPES OF TWO FUNCTION DECLARATIONS ARE DIFFERENT.

   declare
      I : Integer;
      B : Boolean;
      S : String (1 .. 2) := "12";

      function F return Integer is
      begin
         S (1) := 'A';
         return Ident_Int (0); -- THIS VALUE IS IRRELEVENT.
      end F;

      function F return Boolean is
      begin
         S (2) := 'B';
         return Ident_Bool (True); -- THIS VALUE IS IRRELEVANT.
      end F;

   begin
      I := F;
      B := F;

      if S /= "AB" then
         Failed
           ("FUNCTIONS DIFFERING ONLY IN " &
            "BASE TYPE OF RETURNED VALUE " &
            "CAUSED CONFUSION");
      end if;
   end;

   --------------------------------------------------

   Result;

end C66002g;
