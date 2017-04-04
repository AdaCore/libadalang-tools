-- C95095E.ADA

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

--     (E) A SUBPROGRAM IS DECLARED IN AN OUTER DECLARATIVE PART,
--         AN ENTRY IN A TASK, AND ONE HAS ONE MORE PARAMETER
--         THAN THE OTHER; THE OMITTED PARAMETER HAS A DEFAULT VALUE.

-- JWC 7/30/85
-- JRK 10/2/85

with Report; use Report;
procedure C95095e is

begin
   Test
     ("C95095E",
      "SUBPROGRAM/ENTRY OVERLOADING WITH " & "MINIMAL DIFFERENCES ALLOWED");

   --------------------------------------------------

   -- A SUBPROGRAM IS IN AN OUTER DECLARATIVE PART, AN ENTRY IN A TASK, AND ONE
   -- HAS ONE MORE PARAMETER (WITH A DEFAULT VALUE) THAN THE OTHER.

   declare
      S : String (1 .. 3) := "123";

      procedure E (I1, I2, I3 : Integer := 1) is
         C : constant String := "CXA";
      begin
         S (I3) := C (I3);
      end E;

      task T is
         entry E (I1, I2 : Integer := 1);
      end T;

      task body T is
      begin
         accept E (I1, I2 : Integer := 1) do
            S (2) := 'B';
         end E;
      end T;

   begin

      E (1, 2, 3);
      T.E (1, 2);
      E (1, 2);

      if S /= "CBA" then
         Failed
           ("PROCEDURES/ENTRIES DIFFERING " &
            "ONLY IN EXISTENCE OF ONE " &
            "DEFAULT-VALUED PARAMETER CAUSED " &
            "CONFUSION");
      end if;

   end;

   --------------------------------------------------

   Result;
end C95095e;
