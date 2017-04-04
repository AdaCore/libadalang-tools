-- CB5002A.ADA

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
-- OBJECTIVE:
--     CHECK THAT WHEN "TASKING_ERROR" IS RAISED EXPLICITLY OR BY
--     PROPAGATION WITHIN AN ACCEPT STATEMENT, THEN "TASKING_ERROR"
--     IS RAISED IN BOTH THE CALLING AND THE CALLED TASK.

-- HISTORY:
--     DHH 03/31/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure Cb5002a is

begin
   Test
     ("CB5002A",
      "CHECK THAT WHEN ""TASKING_ERROR"" IS RAISED " &
      "EXPLICITLY OR BY PROPAGATION WITHIN AN ACCEPT " &
      "STATEMENT, THEN ""TASKING_ERROR"" IS RAISED " &
      "IN BOTH THE CALLING AND THE CALLED TASK");

   declare
      task Calling_Exp is
         entry A;
      end Calling_Exp;

      task Called_Exp is
         entry B;
         entry Stop;
      end Called_Exp;

      task Calling_Prop is
         entry C;
      end Calling_Prop;

      task Called_Prop is
         entry D;
         entry Stop;
      end Called_Prop;

      task Prop is
         entry E;
         entry Stop;
      end Prop;
-----------------------------------------------------------------------
      task body Calling_Exp is
      begin
         accept A do
            begin
               Called_Exp.B;
               Failed
                 ("EXCEPTION NOT RAISED IN CALLING " &
                  "TASK - EXPLICIT RAISE");
            exception
               when Tasking_Error =>
                  null;
               when others =>
                  Failed
                    ("WRONG EXCEPTION RAISED IN " &
                     "CALLING TASK - EXPLICIT RAISE");
            end; -- EXCEPTION
         end A;
      end Calling_Exp;

      task body Called_Exp is
      begin
         begin
            accept B do
               raise Tasking_Error;
               Failed
                 ("EXCEPTION NOT RAISED IN CALLED " & "TASK - EXPLICIT RAISE");
            end B;
         exception
            when Tasking_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED IN CALLED " &
                  "TASK - EXPLICIT RAISE");
         end;  -- EXCEPTION BLOCK

         accept Stop;
      end Called_Exp;

-----------------------------------------------------------------------
      task body Calling_Prop is
      begin
         accept C do
            begin
               Called_Prop.D;
               Failed
                 ("EXCEPTION NOT RAISED IN CALLING " &
                  "TASK - PROPAGATED RAISE");
            exception
               when Tasking_Error =>
                  null;
               when others =>
                  Failed
                    ("WRONG EXCEPTION RAISED IN " &
                     "CALLING TASK - PROPAGATED RAISE");
            end;  -- EXCEPTION
         end C;
      end Calling_Prop;

      task body Called_Prop is
      begin
         begin
            accept D do
               Prop.E;
               Failed
                 ("EXCEPTION NOT RAISED IN CALLED " &
                  "TASK - PROPAGATED RAISE");
            end D;
         exception
            when Tasking_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED IN CALLED " &
                  "TASK - PROPAGATED RAISE");
         end;  -- EXCEPTION BLOCK;

         accept Stop;
      end Called_Prop;

      task body Prop is
      begin
         begin
            accept E do
               raise Tasking_Error;
               Failed
                 ("EXCEPTION NOT RAISED IN PROPAGATE " & "TASK - ACCEPT E");
            end E;
         exception
            when Tasking_Error =>
               null;
            when others =>
               Failed ("WRONG EXCEPTION RAISED IN PROP. TASK");
         end;    -- EXCEPTION BLOCK

         accept Stop;

      end Prop;
-----------------------------------------------------------------------
   begin
      Calling_Exp.A;
      Calling_Prop.C;
      Called_Exp.Stop;
      Called_Prop.Stop;
      Prop.Stop;

   end;    -- DECLARE

   Result;
end Cb5002a;
