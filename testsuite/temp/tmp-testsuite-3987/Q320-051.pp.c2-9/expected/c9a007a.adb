-- C9A007A.ADA

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
-- CHECK THAT A TASK MAY ABORT A TASK IT DEPENDS ON.

-- RM 5/26/82
-- RM 7/02/82
-- SPS 11/21/82
-- JBG 2/27/84
-- JBG 3/8/84
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.
-- EDS 08/04/98 ENSURE THAT ABORTED TASKS HAVE TIME TO EFFECT THEIR ABORTIONS.

with Impdef;
with Report; use Report;
with System; use System;
procedure C9a007a is

   Task_Not_Aborted : Boolean := False;
   Test_Valid       : Boolean := True;

begin

   -------------------------------------------------------------------

   Test ("C9A007A", "CHECK THAT A TASK MAY ABORT A TASK" & " IT DEPENDS ON");

   declare

      task Register is

         entry Births_And_Deaths;

         entry Sync1;
         entry Sync2;

      end Register;

      task body Register is

         task type Secondary is

            entry Wait_Indefinitely;

         end Secondary;

         task type T_Type1 is

            entry E;

         end T_Type1;

         task type T_Type2 is

            entry E;

         end T_Type2;

         T_Object1 : T_Type1;
         T_Object2 : T_Type2;

         task body Secondary is
         begin
            Sync1;
            abort T_Object1;
            delay 0.0;
            Task_Not_Aborted := True;
         end Secondary;

         task body T_Type1 is

            type Access_To_Task is access Secondary;

         begin

            declare
               Dependent_By_Access : Access_To_Task := new Secondary;
            begin
               null;
            end;

            Births_And_Deaths;
            -- DURING THIS SUSPENSION
            --     MOST OF THE TASKS
            --     ARE ABORTED   (FIRST
            --     TASK #1    -- T_OBJECT1 --
            --     THEN  #2 ).

            Task_Not_Aborted := True;

         end T_Type1;

         task body T_Type2 is

            task Inner_Task is

               entry Wait_Indefinitely;

            end Inner_Task;

            task body Inner_Task is
            begin
               Sync2;
               abort T_Object2;
               delay 0.0;
               Task_Not_Aborted := True;
            end Inner_Task;

         begin

            Births_And_Deaths;
            -- DURING THIS SUSPENSION
            --     MOST OF THE TASKS
            --     ARE ABORTED   (FIRST
            --     TASK #1     -- T_OBJECT1 --
            --     THEN  #2 ).

            Task_Not_Aborted := True;

         end T_Type2;

      begin

         declare
            Old_Count : Integer := 0;
         begin

            for I in 1 .. 5 loop
               exit when Births_And_Deaths'Count = 2;
               delay 10.0;
            end loop;

            Old_Count := Births_And_Deaths'Count;

            if Old_Count = 2 then

               accept Sync1;   -- ALLOWING  ABORT#1

               delay Impdef.Clear_Ready_Queue;

               -- CHECK THAT  #1  WAS ABORTED  -  3 WAYS:

               begin
                  T_Object1.E;
                  Failed ("T_OBJECT1.E  DID NOT RAISE" & "  TASKING_ERROR");
               exception

                  when Tasking_Error =>
                     null;

                  when others =>
                     Failed ("OTHER EXCEPTION RAISED - 1");

               end;

               if T_Object1'Callable then
                  Failed ("T_OBJECT1'CALLABLE = TRUE");
               end if;

               if Old_Count - Births_And_Deaths'Count /= 1 then
                  Failed ("TASK#1 NOT REMOVED FROM QUEUE");
               end if;

               Old_Count := Births_And_Deaths'Count;

               accept Sync2;   -- ALLOWING  ABORT#2

               delay Impdef.Clear_Ready_Queue;

               -- CHECK THAT  #2  WAS ABORTED  -  3 WAYS:

               begin
                  T_Object2.E;
                  Failed ("T_OBJECT2.E  DID NOT RAISE" & "  TASKING_ERROR");
               exception

                  when Tasking_Error =>
                     null;

                  when others =>
                     Failed ("OTHER EXCEPTION RAISED - 2");

               end;

               if T_Object2'Callable then
                  Failed ("T_OBJECT2'CALLABLE = TRUE");
               end if;

               if Old_Count - Births_And_Deaths'Count /= 1 then
                  Failed ("TASK#2 NOT REMOVED FROM QUEUE");
               end if;

               if Births_And_Deaths'Count /= 0 then
                  Failed ("SOME TASKS STILL QUEUED");
               end if;

            else

               Comment ("LINEUP NOT COMPLETE (AFTER 50 S.)");
               Test_Valid := False;

            end if;

         end;

         while Births_And_Deaths'Count > 0 loop
            accept Births_And_Deaths;
         end loop;

      end Register;

   begin

      null;

   end;

   -------------------------------------------------------------------

   if Test_Valid and Task_Not_Aborted then
      Failed ("SOME TASKS NOT ABORTED");
   end if;

   Result;

end C9a007a;
