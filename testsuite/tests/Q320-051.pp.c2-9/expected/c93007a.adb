-- C93007A.ADA

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
--     CHECK THAT IF AN ATTEMPT IS MADE TO ACTIVATE A TASK BEFORE ITS
--     BODY HAS BEEN ELABORATED, THE TASK IS COMPLETED AND "PROGRAM_
--     ERROR" (RATHER THAN "TASKING_ERROR") IS RAISED.

-- HISTORY:
--     DHH 03/16/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C93007a is

begin

   Test
     ("C93007A",
      "CHECK THAT IF AN ATTEMPT IS MADE TO ACTIVATE " &
      "A TASK BEFORE ITS BODY HAS BEEN ELABORATED, " &
      "THE TASK IS COMPLETED AND ""PROGRAM_ERROR"" " &
      "(RATHER THAN ""TASKING_ERROR"") IS RAISED");

   declare
      task type Prog_Err is
         entry Start;
      end Prog_Err;

      type Rec is record
         B : Prog_Err;
      end record;

      type Acc is access Prog_Err;

      package P is
         Obj : Rec;
      end P;

      package body P is
      begin
         Failed ("EXCEPTION NOT RAISED - 1");
         Obj.B.Start;
      exception
         when Program_Error =>
            null;
         when Tasking_Error =>
            Failed ("TASKING ERROR RAISED INCORRECTLY");
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED");
      end P;

      package Q is
         Obj : Acc;
      end Q;

      package body Q is
      begin
         Obj := new Prog_Err;
         Failed ("EXCEPTION NOT RAISED - 2");
         Obj.Start;
      exception
         when Program_Error =>
            null;
         when Tasking_Error =>
            Failed ("ACCESS TASKING ERROR RAISED INCORRECTLY");
         when others =>
            Failed ("ACCESS UNEXPECTED EXCEPTION RAISED");
      end Q;

      task body Prog_Err is
      begin
         accept Start do
            if True then
               Comment ("IRRELEVANT");
            end if;
         end Start;
      end Prog_Err;
   begin
      null;
   end; -- DECLARE

   Result;

exception
   when Program_Error =>
      Failed ("PROGRAM_ERROR RAISED AT INCORRECT POSITION");
      Result;

   when others =>
      Failed ("UNEXPECTED EXCEPTION RAISED");
      Result;

end C93007a;
