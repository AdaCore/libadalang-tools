-- C97117B.ADA

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
-- CHECK THAT AN ELSE PART IS EXECUTED IF ALL ALTERNATIVES ARE CLOSED OR IF
-- THERE ARE NO TASKS QUEUED FOR OPEN ALTERNATIVES.

-- WRG 7/10/86

with Report; use Report;
procedure C97117b is

begin

   Test
     ("C97117B",
      "CHECK THAT AN ELSE PART IS EXECUTED IF ALL " &
      "ALTERNATIVES ARE CLOSED OR IF THERE ARE NO " &
      "TASKS QUEUED FOR OPEN ALTERNATIVES");

   declare

      task T is
         entry E;
         entry No_Go;
      end T;

      task body T is
      begin
         -- ENSURE THAT NO_GO HAS BEEN CALLED BEFORE PROCEEDING:
         while No_Go'Count = 0 loop
            delay 1.0;
         end loop;

         select when Ident_Bool (False) =>
            accept E;
            Failed
              ("CLOSED ACCEPT ALTERNATIVE TAKEN " &
               "FOR NONEXISTENT ENTRY CALL - 1");
         or when Ident_Bool (False) =>
            accept No_Go;
            Failed ("CLOSED ALTERNATIVE TAKEN - 1");
         else
            Comment ("ELSE PART EXECUTED - 1");
         end select;

         select
            accept E;
            Failed ("ACCEPTED NONEXISTENT ENTRY CALL - 2");
         or when Ident_Bool (False) =>
            accept No_Go;
            Failed ("CLOSED ALTERNATIVE TAKEN - 2");
         else
            Comment ("ELSE PART EXECUTED - 2");
         end select;

         accept No_Go;
      end T;

   begin

      T.No_Go;

   end;

   Result;

end C97117b;
