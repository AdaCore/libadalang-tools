-- C97117C.ADA

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
-- CHECK THAT AN ELSE PART IS NOT EXECUTED IF A TASK IS QUEUED AT AN
-- OPEN ALTERNATIVE.

-- WRG 7/10/86

with Report; use Report;
procedure C97117c is

begin

   Test
     ("C97117C",
      "CHECK THAT AN ELSE PART IS NOT EXECUTED IF A " &
      "TASK IS QUEUED AT AN OPEN ALTERNATIVE");

   declare

      task T is
         entry E;
         entry No_Go;
      end T;

      task body T is
      begin
         --ENSURE THAT E HAS BEEN CALLED BEFORE PROCEEDING:
         while E'Count = 0 loop
            delay 1.0;
         end loop;

         select
            accept No_Go;
            Failed ("ACCEPTED NONEXISTENT ENTRY CALL");
         or when Ident_Bool (True) =>
            accept E;
         or when Ident_Bool (False) =>
            accept E;
            Failed ("CLOSED ALTERNATIVE TAKEN");
         else
            Failed ("ELSE PART EXECUTED");
         end select;
      end T;

   begin

      T.E;

   end;

   Result;

end C97117c;
