-- C66002F.ADA

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

--     (F) ONE SUBPROGRAM IS DECLARED IN AN OUTER DECLARATIVE PART,
--         THE OTHER IN AN INNER PART, AND ONE HAS ONE MORE PARAMETER
--         THAN THE OTHER; THE OMITTED PARAMETER HAS A DEFAULT VALUE.

-- CVP 5/4/81
-- JRK 5/8/81
-- NL 10/13/81

with Report;
procedure C66002f is

   use Report;

begin
   Test
     ("C66002F",
      "SUBPROGRAM OVERLOADING WITH " & "MINIMAL DIFFERENCES ALLOWED");

   --------------------------------------------------

   -- ONE SUBPROGRAM IS IN AN OUTER DECLARATIVE PART, THE OTHER IN AN INNER
   -- PART, AND ONE HAS ONE MORE PARAMETER (WITH A DEFAULT VALUE) THAN THE
   -- OTHER.

   Bf :
   declare
      S : String (1 .. 3) := "123";

      procedure P (I1, I2, I3 : Integer := 1) is
         C : constant String := "CXA";
      begin
         S (I3) := C (I3);
      end P;

      procedure Enclose is

         procedure P (I1, I2 : Integer := 1) is
         begin
            S (2) := 'B';
         end P;

      begin -- ENCLOSE
         P (1, 2, 3);
         Enclose.P (1, 2); -- NOTE THAT THESE CALLS
         Bf.P (1, 2);      -- MUST BE DISAMBIGUATED.

         if S /= "CBA" then
            Failed
              ("PROCEDURES IN ENCLOSING-" &
               "ENCLOSED SCOPES DIFFERING " &
               "ONLY IN EXISTENCE OF ONE " &
               "DEFAULT-VALUED PARAMETER CAUSED " &
               "CONFUSION");
         end if;
      end Enclose;

   begin
      Enclose;
   end Bf;

   --------------------------------------------------

   Result;

end C66002f;
