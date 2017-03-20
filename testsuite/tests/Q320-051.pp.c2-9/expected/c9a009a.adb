-- C9A009A.ADA

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
-- TEST ABORT DURING RENDEZVOUS

-- CALLING TASK IN RENDEVOUS IS NAMED IN ABORT STATEMENT.

-- JEAN-PIERRE ROSEN 09 MARCH 1984
-- JBG 6/1/84
-- JWC 6/28/85   RENAMED FROM C9A009D-B.ADA

with System; use System;
with Report; use Report;
procedure C9a009a is

begin

   Test ("C9A009A", "CALLING TASK IS ABORTED DIRECTLY");

   declare
      -- T1 CALLS T2, WHICH ABORTS T1 WHILE IN RENDEVOUS

      T2_Continued : Boolean := False;

      task Continued is
         entry Get (T2_Continued : out Boolean);
         entry Put (T2_Continued : in Boolean);
      end Continued;

      task body Continued is
         Continued : Boolean := False;
      begin
         loop
            select
               accept Get (T2_Continued : out Boolean) do
                  T2_Continued := Continued;
               end Get;
            or
               accept Put (T2_Continued : in Boolean) do
                  Continued := T2_Continued;
               end Put;
            or
               terminate;
            end select;
         end loop;
      end Continued;

   begin     -- THIS BLOCK WILL MAKE SURE T2 IS TERMINATED, AND SO,
      -- T2_CONTINUED IS ASSIGNED A VALUE IF T2 CONTINUES
      -- EXECUTION CORRECTLY.

      declare

         task T1;

         task T2 is
            entry E1;
         end T2;

         task body T1 is
         begin
            T2.E1;
            Failed ("T1 NOT ABORTED");
         exception
            when Tasking_Error =>
               Failed ("TASKING_ERROR RAISED IN T1");
            when others =>
               Failed ("OTHER EXCEPTION RAISED - T1");
         end T1;

         task body T2 is
         begin
            accept E1 do
               abort T1;
               abort T1;
               abort T1; -- WHY NOT?
               if T1'Terminated then
                  Failed ("T1 PREMATURELY TERMINATED");
               end if;
            end E1;
            Continued.Put (T2_Continued => True);
         end T2;
      begin
         null;
      end;
      -- T2 NOW TERMINATED
      Continued.Get (T2_Continued);
      if not T2_Continued then
         Failed
           ("WHEN CALLER WAS ABORTED IN RENDEVOUS, CALLED " &
            "TASK DID NOT CONTINUE");
      end if;
   end;

   Result;

end C9a009a;
