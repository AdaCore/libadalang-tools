-- C97202A.ADA

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
-- CHECK THAT THE INDEX IS EVALUATED BEFORE THE ENTRY PARAMETER AND BOTH
-- THE INDEX AND THE ENTRY PARAMETER ARE EVALUATED BEFORE THE RENDEZVOUS
-- IS ATTEMPED.

-- RM 4/05/82
-- TBN 2/3/86     ADDED A CHECK THAT INDEX IS EVALUATED BEFORE THE ENTRY
--                PARAMETER AND FIXED APPROPRIATE COMMENTS.

with Report; use Report;
procedure C97202a is

   Index_Computed  : Boolean := False;
   Formal_Computed : Boolean := False;

begin

   Test
     ("C97202A",
      "CHECK THAT THE INDEX IS EVALUATED BEFORE THE " &
      "ENTRY PARAMETER AND BOTH INDEX AND THE ENTRY " &
      "PARAMETER ARE EVALUATED BEFORE THE RENDEZVOUS " &
      "IS ATTEMPTED");

   declare
      subtype Short is Integer range 10 .. 20;

      task T is
         entry Do_It_Now_Orelse (Short) (Did_You_Do_It : in Boolean);
         entry Keep_Alive;
      end T;

      task body T is
      begin
         accept Keep_Alive;
      end T;

      function F1 (X : Integer) return Integer is
      begin
         if Formal_Computed then
            Failed ("INDEX WAS NOT EVALUATED FIRST");
         end if;
         Index_Computed := True;
         return (7);
      end F1;

      function F2 (X : Integer) return Boolean is
      begin
         Formal_Computed := True;
         return (False);
      end F2;

   begin
      select
         T.Do_It_Now_Orelse (6 + F1 (7)) (not (F2 (7)));
      else
         null;
      end select;

      T.Keep_Alive;
   end;   -- END OF BLOCK CONTAINING THE ENTRY CALLS.

   if Index_Computed then
      null;
   else
      Failed ("ENTRY INDEX WAS NOT COMPUTED");
   end if;

   if Formal_Computed then
      null;
   else
      Failed ("ENTRY PARAMETER WAS NOT COMPUTED");
   end if;

   Result;

end C97202a;
