-- C74409B.ADA

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
-- CHECK THAT IF A COMPOSITE TYPE IS DECLARED IN THE SAME PACKAGE
-- AS A LIMITED PRIVATE TYPE AND HAS A COMPONENT OF THAT TYPE,
-- THE COMPOSITE TYPE IS TREATED AS A LIMITED TYPE UNTIL THE
-- EARLIEST PLACE WITHIN THE IMMEDIATE SCOPE OF THE DECLARATION
-- OF THE COMPOSITE TYPE AND AFTER THE FULL DECLARATION OF THE
-- LIMITED PRIVATE TYPE

-- DSJ 5/5/83
-- JBG 9/23/83

with Report;
procedure C74409b is

   use Report;

begin

   Test
     ("C74409B",
      "CHECK THAT A COMPOSITE TYPE WITH A LIMITED " &
      "PRIVATE COMPONENT IS TREATED AS A LIMITED " &
      "TYPE UNTIL ASSIGNMENT AND EQUALITY ARE BOTH " &
      "AVAILABLE FOR THE COMPOSITE TYPE");

   declare

      package P is
         type Lp is limited private;
         package Q is
            type Lp_Array is array (1 .. 2) of Lp;
         end Q;
      private
         type Lp is new Integer;
      end P;

      package body P is
         use Q;
         function "=" (L, R : Lp_Array) return Boolean is  -- LEGAL
         begin
            return True;
         end "=";

         generic
            type T is private;     -- NOTE: NOT LIMITED PRIVATE
            C, D : T;
         package A is
         -- IRRELEVANT DETAILS
         end A;

         package body A is
         begin
            if C = D then
               Failed ("USED WRONG EQUALITY OPERATOR");
            end if;
         end A;

         package body Q is
            package Another_New_A is new A (Lp_Array, (2, 3), (4, 5)); -- LEGAL
         end Q;
      end P;

   begin

      null;

   end;

   Result;

end C74409b;
