-- C9A003A.ADA

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
-- CHECK THAT ABORTING A TERMINATED TASK DOES NOT CAUSE EXCEPTIONS.

-- RM 5/21/82
-- SPS 11/21/82
-- PWN 09/11/94  REMOVED PRAGMA PRIORITY FOR ADA 9X

with Report; use Report;
with System; use System;
procedure C9a003a is

-- THE TASK WILL HAVE HIGHER PRIORITY ( PRIORITY'LAST )

begin

   -------------------------------------------------------------------

   Test
     ("C9A003A",
      "CHECK THAT  ABORTING A TERMINATED TASK" &
      "  DOES NOT CAUSE EXCEPTIONS");

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

   begin

      if not T_Object1'Terminated then
         delay 20.0;
      end if;

      if not T_Object1'Terminated then
         Comment ("TASK NOT YET TERMINATED (AFTER 20 S.)");
      end if;

      begin
         abort T_Object1;
      exception

         when others =>
            Failed
              ("EXCEPTION RAISED (WHEN ABORTING A" & "  TERMINATED TASK)");

      end;

   end;

   -------------------------------------------------------------------

   Result;

end C9a003a;
