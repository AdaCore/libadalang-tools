-- C97201G.ADA

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
-- CHECK THAT A RENDEZVOUS REQUESTED BY A CONDITIONAL_ENTRY_CALL
--     IS PERFORMED ONLY IF IMMEDIATELY POSSIBLE.

-- CASE G: THE CORRESPONDING ACCEPT_STATEMENT IS CLOSED
--     AND THIS FACT IS STATICALLY DETERMINABLE.

-- RM 4/21/82

with Report; use Report;
procedure C97201g is

   Else_Branch_Taken   : Boolean := False;
   Rendezvous_Occurred : Boolean := False;
   Queue_Not_Empty     : Boolean := False;
   X                   : Integer := 17;

begin

   Test
     ("C97201G",
      "CHECK THAT NO RENDEZVOUS REQUESTED BY" &
      " A CONDITIONAL_ENTRY_CALL CAN EVER OCCUR" &
      " IF THE CORRESPONDING ACCEPT_STATEMENT IS" & " CLOSED");

   -------------------------------------------------------------------

   declare

      task T is
         entry Do_It_Now_Orelse (Did_You_Do_It : in out Boolean);
         entry Keep_Alive;
      end T;

      task body T is
      begin

         if Do_It_Now_Orelse'Count /= 0 then
            Queue_Not_Empty := True;
         end if;

         select when 3 = 5 =>
            accept Do_It_Now_Orelse (Did_You_Do_It : in out Boolean) do
               Did_You_Do_It := True;
            end Do_It_Now_Orelse;
         or
            accept Keep_Alive; -- TO PREVENT SELECT_ERROR
         end select;

         if Do_It_Now_Orelse'Count /= 0 then
            Queue_Not_Empty := True;
         end if;

      end T;

   begin

      Comment ("PERMANENTLY CLOSED");

      select
         T.Do_It_Now_Orelse (Rendezvous_Occurred);
      else              -- (I.E. CALLER ADOPTS A NO-WAIT POLICY)
         --      THEREFORE THIS BRANCH MUST BE CHOSEN
         Else_Branch_Taken := True;
         Comment ("ELSE_BRANCH  TAKEN");
      end select;

      T.Keep_Alive;    -- THIS ALSO UPDATES THE NONLOCALS

   end;   -- END OF BLOCK CONTAINING THE ENTRY CALL

   -------------------------------------------------------------------

   -- BY NOW, THE TASK IS TERMINATED

   if Rendezvous_Occurred then
      Failed ("RENDEZVOUS OCCURRED");
   end if;

   if Queue_Not_Empty then
      Failed ("ENTRY QUEUE NOT EMPTY");
   end if;

   if Else_Branch_Taken then
      null;
   else
      Failed ("RENDEZVOUS ATTEMPTED?");
   end if;

   Result;

end C97201g;
