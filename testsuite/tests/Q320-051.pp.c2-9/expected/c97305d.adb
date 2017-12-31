-- C97305D.ADA

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
-- CHECK THAT IF THE RENDEZVOUS IS NOT IMMEDIATELY POSSIBLE BUT BECOMES
-- POSSIBLE BEFORE THE DELAY EXPIRES, THE TIMED ENTRY CALL IS ACCEPTED.

-- CASE B: ENTRY FAMILY; THE CALLED TASK IS EXECUTING A SELECTIVE WAIT.

-- WRG 7/13/86
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

with Report; use Report;
with System; use System;
procedure C97305d is

   Rendezvous_Occurred            : Boolean           := False;
   Statements_After_Call_Executed : Boolean           := False;
   Delay_In_Minutes               : constant Positive := 30;

begin

   Test
     ("C97305D",
      "CHECK THAT IF THE RENDEZVOUS IS NOT " &
      "IMMEDIATELY POSSIBLE BUT BECOMES POSSIBLE " &
      "BEFORE THE DELAY EXPIRES, THE TIMED ENTRY " & "CALL IS ACCEPTED");

   declare

      task T is
         entry E (1 .. 3) (B : in out Boolean);
      end T;

      task body T is
      begin
         delay 10.0;

         select
            accept E (2) (B : in out Boolean) do
               B := Ident_Bool (True);
            end E;
         or
            accept E (3) (B : in out Boolean);
            Failed ("NONEXISTENT ENTRY CALL ACCEPTED");
         end select;
      end T;

   begin

      select
         T.E (2) (Rendezvous_Occurred);
         Statements_After_Call_Executed := Ident_Bool (True);
      or
         delay Delay_In_Minutes * 60.0;
         Failed
           ("TIMED ENTRY CALL NOT ACCEPTED AFTER" &
            Positive'Image (Delay_In_Minutes) & " MINUTES ELAPSED");

      end select;

   end;

   if not Rendezvous_Occurred then
      Failed ("RENDEZVOUS DID NOT OCCUR");
   end if;

   if not Statements_After_Call_Executed then
      Failed ("STATEMENTS AFTER ENTRY CALL NOT EXECUTED");
   end if;

   Result;

end C97305d;
