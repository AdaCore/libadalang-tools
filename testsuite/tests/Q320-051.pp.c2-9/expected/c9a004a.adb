-- C9A004A.ADA

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
-- CHECK THAT IF A TASK IS ABORTED BEFORE BEING ACTIVATED, THE TASK IS
--     TERMINATED.

-- RM 5/21/82
-- SPS 11/21/82
-- JBG 6/3/85
-- PWN 09/11/94 REMOVED PRAGMA PRIORITY FOR ADA 9X

with Report; use Report;
with System; use System;
procedure C9a004a is

begin

   -------------------------------------------------------------------

   Test
     ("C9A004A",
      "CHECK THAT IF A TASK IS ABORTED" &
      " BEFORE BEING ACTIVATED," &
      "  THE TASK IS TERMINATED");

   declare

      task type T_Type is

         entry E;

      end T_Type;

      T_Object1 : T_Type;

      task body T_Type is
         Busy : Boolean := False;
      begin

         null;

      end T_Type;

      package P is
         X : Integer := 0;
      end P;

      package body P is
      begin

         if T_Object1'Terminated or not T_Object1'Callable then
            Failed ("WRONG VALUES FOR ATTRIBUTES");
         end if;

         abort T_Object1;  -- ELABORATED BUT NOT YET ACTIVATED.

      end P;

   begin

      if not T_Object1'Terminated then
         Failed ("ABORTED (BEFORE ACTIVATION) TASK" & "  NOT TERMINATED");
      end if;

   exception

      when Tasking_Error =>
         Failed ("TASKING_ERROR RAISED");

   end;

   Result;

end C9a004a;
