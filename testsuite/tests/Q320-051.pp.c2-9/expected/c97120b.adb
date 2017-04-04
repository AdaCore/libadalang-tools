-- C97120B.ADA

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
-- CHECK THAT IF A SPECIFIED DELAY IS ZERO OR NEGATIVE AND AN ENTRY CALL IS
-- WAITING AT AN OPEN ALTERNATIVE WHEN THE SELECTIVE WAIT IS EXECUTED, THE
-- CALL IS ACCEPTED.

-- WRG 7/11/86

with Report; use Report;
procedure C97120b is

   Zero, Neg : Duration := 1.0;

begin

   Test
     ("C97120B",
      "CHECK THAT IF A SPECIFIED DELAY IS ZERO OR " &
      "NEGATIVE AND AN ENTRY CALL IS WAITING AT AN " &
      "OPEN ALTERNATIVE WHEN THE SELECTIVE WAIT IS " &
      "EXECUTED, THE CALL IS ACCEPTED");

   if Equal (3, 3) then
      Zero := 0.0;
      Neg  := -1.0;
   end if;

   declare

      task T is
         entry E;
      end T;

      task body T is
      begin
         while E'Count = 0 loop
            delay 1.0;
         end loop;

         A : begin
            select when Ident_Bool (True) =>
               accept E;
            or
               delay Zero;
               Failed ("ZERO DELAY ALTERNATIVE TAKEN");
               accept E;
            end select;
         exception
            when others =>
               Failed ("EXCEPTION RAISED (A)");
         end A;

         while E'Count = 0 loop
            delay 1.0;
         end loop;

         B : begin
            select
               accept E;
            or
               delay Neg;
               Failed ("NEGATIVE DELAY ALTERNATIVE TAKEN");
               accept E;
            end select;
         exception
            when others =>
               Failed ("EXCEPTION RAISED (B)");
         end B;

      end T;

   begin

      T.E;
      T.E;

   end;

   Result;

end C97120b;
