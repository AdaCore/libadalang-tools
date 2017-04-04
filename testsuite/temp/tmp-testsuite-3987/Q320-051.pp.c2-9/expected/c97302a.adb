-- C97302A.ADA

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
-- CHECK THAT WHENEVER AN INDEX IS PRESENT IN A TIMED_ENTRY_CALL, IT
-- IS EVALUATED BEFORE ANY PARAMETER ASSOCIATIONS ARE EVALUATED, AND
-- PARAMETER ASSOCIATIONS ARE EVALUATED BEFORE THE DELAY EXPRESSION.
-- THEN A RENDEZVOUS IS ATTEMPTED.

-- RJW 3/31/86

with Report;   use Report;
with Calendar; use Calendar;
procedure C97302a is

   Index_Computed : Boolean := False;
   Param_Computed : Boolean := False;
   Delay_Computed : Boolean := False;
begin

   Test
     ("C97302A",
      "CHECK THAT WHENEVER AN INDEX IS PRESENT IN " &
      "A TIMED_ENTRY_CALL, IT IS EVALUATED BEFORE " &
      "ANY PARAMETER ASSOCIATIONS ARE EVALUATED, " &
      "AND PARAMETER ASSOCIATIONS ARE EVALUATED " &
      "BEFORE THE DELAY EXPRESSION");
   declare

      Wait_Time : Duration := 3.0;

      type Short is range 10 .. 20;

      task T is
         entry Do_It_Now_Or_Wait (Short) (Did_You_Do_It : in Boolean);
         entry Keep_Alive;
      end T;

      task body T is
      begin
         accept Keep_Alive;
      end T;

      function F1 (X : Short) return Short is
      begin
         Index_Computed := True;
         return (15);
      end F1;

      function F2 return Boolean is
      begin
         if Index_Computed then
            null;
         else
            Failed ("INDEX NOT EVALUATED FIRST");
         end if;
         Param_Computed := True;
         return (False);
      end F2;

      function F3 return Duration is
      begin
         if Param_Computed then
            null;
         else
            Failed ("PARAMETERS NOT EVALUATED BEFORE DELAY " & "EXPRESSION");
         end if;
         Delay_Computed := True;
         return (Wait_Time);
      end F3;
   begin

      select
         T.Do_It_Now_Or_Wait (F1 (15)) (not F2);
         Failed ("RENDEZVOUS OCCURRED");
      or
         delay F3;
      end select;

      T.Keep_Alive;

   end;   -- END OF BLOCK CONTAINING THE ENTRY CALLS.

   if Delay_Computed then
      null;
   else
      Failed ("DELAY EXPRESSION NOT EVALUATED");
   end if;

   Result;

end C97302a;
