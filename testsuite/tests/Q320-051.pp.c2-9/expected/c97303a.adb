-- C97303A.ADA

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
-- CHECK THAT A TIMED_ENTRY_CALL CAN APPEAR IN PLACES WHERE A
--     SELECTIVE_WAIT  CANNOT.

-- PART 1: PACKAGE BODY EMBEDDED IN TASK BODY.

-- RM 4/06/1982

with Report; use Report;
procedure C97303a is

begin

   Test
     ("C97303A",
      "CHECK THAT A  TIMED_ENTRY_CALL  CAN" &
      " APPEAR WHERE A  SELECTIVE_WAIT  CANNOT");

   -------------------------------------------------------------------

   declare

      task Tt is
         entry A (Authorized : in Boolean);
      end Tt;

      task body Tt is

         package Within_Task_Body is
         -- NOTHING HERE
         end Within_Task_Body;

         package body Within_Task_Body is
         begin

            select  -- NOT A SELECTIVE_WAIT
               A (False);  -- CALLING (OWN) ENTRY
            or
               delay 1.0;
               Comment ("ALTERNATIVE BRANCH TAKEN");
            end select;

         end Within_Task_Body;

      begin

         accept A (Authorized : in Boolean) do

            if Authorized then
               Comment ("AUTHORIZED ENTRY_CALL");
            else
               Failed ("UNAUTHORIZED ENTRY_CALL");
            end if;

         end A;

      end Tt;

      package Outside_Task_Body is
      -- NOTHING HERE
      end Outside_Task_Body;

      package body Outside_Task_Body is
      begin

         select  -- NOT A SELECTIVE_WAIT
            Tt.A (False);  -- UNBORN
         or
            delay 2.0;
            Comment ("(OUT:) ALTERNATIVE BRANCH TAKEN");
         end select;

      end Outside_Task_Body;

   begin

      Tt.A (True);

   exception

      when Tasking_Error =>
         Failed ("TASKING ERROR");

   end;

   -------------------------------------------------------------------

   Result;

end C97303a;
