-- C97201A.ADA

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

-- CASE A: THE TASK TO BE CALLED IS NOT YET ACTIVE AS OF THE
--     MOMENT OF CALL (CONDITIONAL_ENTRY_CALL),
--     AND THIS FACT CAN BE DETERMINED STATICALLY.

-- RM 4/20/82

with Report; use Report;
procedure C97201a is

   Else_Branch_Taken : Integer := 3;

begin

   Test
     ("C97201A",
      "CHECK THAT NO RENDEZVOUS REQUESTED BY" &
      " A CONDITIONAL_ENTRY_CALL CAN OCCUR  WHILE" &
      " THE CALLED TASK IS NOT YET ACTIVE");

   -------------------------------------------------------------------

   declare

      task T is
         entry Do_It_Now_Orelse (Authorized : in Boolean);
      end T;

      task body T is

         package Second_Attempt is
         end Second_Attempt;
         package body Second_Attempt is
         begin

            select
               Do_It_Now_Orelse (False);--CALLING (OWN) ENTRY
            else    -- (I.E. CALLER ADOPTS A NO-WAIT POLICY)
               --      THEREFORE THIS BRANCH MUST BE CHOSEN
               Else_Branch_Taken := 2 * Else_Branch_Taken;
               Comment ("ELSE_BRANCH  TAKEN  (#2)");
            end select;

         end Second_Attempt;

      begin

         accept Do_It_Now_Orelse (Authorized : in Boolean) do

            if Authorized then
               Comment ("AUTHORIZED ENTRY_CALL");
            else
               Failed ("UNAUTHORIZED ENTRY_CALL");
            end if;

         end Do_It_Now_Orelse;

      end T;

      package First_Attempt is
      end First_Attempt;
      package body First_Attempt is
      begin
         select
            T.Do_It_Now_Orelse (False);
         else        -- (I.E. CALLER ADOPTS A NO-WAIT POLICY)
            --      THEREFORE THIS BRANCH MUST BE CHOSEN
            Else_Branch_Taken := 1 + Else_Branch_Taken;
            Comment ("ELSE_BRANCH  TAKEN  (#1)");
         end select;

      end First_Attempt;

   begin

      T.Do_It_Now_Orelse (True);   -- TO SATISFY THE SERVER'S
      --     WAIT FOR SUCH A CALL

   exception

      when Tasking_Error =>
         Failed ("TASKING ERROR");

   end;

   -------------------------------------------------------------------

   -- BY NOW, THE TASK IS TERMINATED (AND THE NONLOCALS UPDATED)

   case Else_Branch_Taken is

      when 3 =>
         Failed ("NO 'ELSE'; BOTH (?) RENDEZVOUS ATTEMPTED?");

      when 4 =>
         Failed ("'ELSE' #1 ONLY; RENDEZVOUS (#2) ATTEMPTED?");

      when 6 =>
         Failed ("'ELSE' #2 ONLY; RENDEZVOUS (#1) ATTEMPTED?");

      when 7 =>
         Failed ("WRONG ORDER FOR 'ELSE':  #2,#1 ");

      when 8 =>
         null;

      when others =>
         Failed ("WRONG CASE_VALUE");

   end case;

   Result;

end C97201a;
