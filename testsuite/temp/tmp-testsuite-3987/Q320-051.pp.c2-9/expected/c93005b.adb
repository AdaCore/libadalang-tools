-- C93005B.ADA

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
-- CHECK THAT WHEN AN EXCEPTION IS RAISED IN A DECLARATIVE PART, A TASK
-- DECLARED IN THE SAME DECLARATIVE PART BECOMES TERMINATED.

-- CHECK THAT A TASK WAITING ON ENTRIES OF SUCH A
-- TERMINATED-BEFORE-ACTIVATION TASK RECEIVES TASKING_ERROR.

-- THIS TEST CHECKS THE CASE IN WHICH SEVERAL TASKS ARE WAITING FOR
-- ACTIVATION WHEN THE EXCEPTION OCCURS.

-- R. WILLIAMS 8/7/86
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

with System; use System;
with Report; use Report;

procedure C93005b is

begin
   Test
     ("C93005B",
      "CHECK THAT WHEN AN EXCEPTION IS RAISED IN A " &
      "DECLARATIVE PART, A TASK DECLARED IN THE " &
      "SAME DECLARATIVE PART BECOMES TERMINATED. " &
      "IN THIS CASE, SEVERAL TASKS ARE WAITING FOR " &
      "ACTIVATION WHEN THE EXCEPTION OCCURS");

   begin

      declare
         task type Ta is      -- CHECKS THAT TX TERMINATES.
         end Ta;

         type Ata is access Ta;

         task type Tb is      -- CHECKS THAT TY TERMINATES.
         end Tb;

         type Tbrec is record
            Ttb : Tb;
         end record;

         task Tx is          -- WILL NEVER BE ACTIVATED.
            entry E;
         end Tx;

         task body Ta is
         begin
            declare  -- THIS BLOCK TO CHECK THAT TAB
               -- TERMINATES.
               task Tab is
               end Tab;

               task body Tab is
               begin
                  Tx.E;
                  Failed ("RENDEZVOUS COMPLETED " & "WITHOUT ERROR - TAB");
               exception
                  when Tasking_Error =>
                     null;
                  when others =>
                     Failed ("ABNORMAL EXCEPTION " & "- TAB");
               end Tab;
            begin
               null;
            end;

            Tx.E;    --TX IS NOW TERMINATED.

            Failed ("RENDEZVOUS COMPLETED WITHOUT ERROR " & "- TA");

         exception
            when Tasking_Error =>
               null;
            when others =>
               Failed ("ABNORMAL EXCEPTION - TA");
         end Ta;

         package Raise_It is
            task Ty is             -- WILL NEVER BE ACTIVATED.
               entry E;
            end Ty;
         end Raise_It;

         task body Tb is
         begin
            declare  -- THIS BLOCK TO CHECK THAT TBB
               -- TERMINATES.
               task Tbb is
               end Tbb;

               task body Tbb is
               begin
                  Raise_It.Ty.E;
                  Failed ("RENDEZVOUS COMPLETED " & "WITHOUT ERROR - TBB");
               exception
                  when Tasking_Error =>
                     null;
                  when others =>
                     Failed ("ABNORMAL EXCEPTION " & "- TBB");
               end Tbb;
            begin
               null;
            end;

            Raise_It.Ty.E;    -- TY IS NOW TERMINATED.

            Failed ("RENDEZVOUS COMPLETED WITHOUT ERROR " & "- TB");

         exception
            when Tasking_Error =>
               null;
            when others =>
               Failed ("ABNORMAL EXCEPTION - TB");
         end Tb;

         package Start_Tc is
         end Start_Tc;

         task body Tx is
         begin
            Failed ("TX ACTIVATED");
            -- IN CASE OF FAILURE.
            loop
               select
                  accept E;
               or
                  terminate;
               end select;
            end loop;
         end Tx;

         package Start_Tz is
            task Tz is             -- WILL NEVER BE ACTIVATED.
               entry E;
            end Tz;
         end Start_Tz;

         package body Start_Tc is
            Tbrec1 : Tbrec;     -- CHECKS THAT TY TERMINATES.

            task Tc is -- CHECKS THAT TZ TERMINATES.
            end Tc;

            task body Tc is
            begin
               declare  -- THIS BLOCK TO CHECK THAT TCB
                  -- TERMINATES.

                  task Tcb is
                  end Tcb;

                  task body Tcb is
                  begin
                     Start_Tz.Tz.E;
                     Failed
                       ("RENDEZVOUS COMPLETED " & "WITHOUT " & "ERROR - TCB");
                  exception
                     when Tasking_Error =>
                        null;
                     when others =>
                        Failed ("ABNORMAL " & "EXCEPTION - TCB");
                  end Tcb;
               begin
                  null;
               end;

               Start_Tz.Tz.E;    -- TZ IS NOW TERMINATED.

               Failed ("RENDEZVOUS COMPLETED WITHOUT " & "ERROR - TC");

            exception
               when Tasking_Error =>
                  null;
               when others =>
                  Failed ("ABNORMAL EXCEPTION - TC");
            end Tc;
         end Start_Tc;     -- TBREC1 AND TC ACTIVATED HERE.

         package body Raise_It is
            Nta : Ata := new Ta;  -- NTA.ALL ACTIVATED HERE.

            task body Ty is
            begin
               Failed ("TY ACTIVATED");
               -- IN CASE OF FAILURE.
               loop
                  select
                     accept E;
                  or
                     terminate;
                  end select;
               end loop;
            end Ty;

            package Xception is
               I : Positive := Ident_Int (0); -- RAISE
               -- CONSTRAINT_ERROR.
            end Xception;

            use Xception;

         begin   -- TY WOULD BE ACTIVATED HERE.

            if I /= Ident_Int (2) or I = Ident_Int (1) + 1 then
               Failed ("PACKAGE DIDN'T RAISE EXCEPTION");
            end if;
         end Raise_It;

         package body Start_Tz is
            task body Tz is
            begin
               Failed ("TZ ACTIVATED");
               -- IN CASE OF FAILURE.
               loop
                  select
                     accept E;
                  or
                     terminate;
                  end select;
               end loop;
            end Tz;
         end Start_Tz;    -- TZ WOULD BE ACTIVATED HERE.

      begin     -- TX WOULD BE ACTIVATED HERE.
         -- CAN'T LEAVE BLOCK UNTIL TA, TB, AND TC ARE TERM.

         Failed ("EXCEPTION NOT RAISED");
      end;

   exception
      when Constraint_Error =>
         null;
      when Tasking_Error =>
         Failed ("TASKING_ERROR IN MAIN PROGRAM");
      when others =>
         Failed ("ABNORMAL EXCEPTION IN MAIN");
   end;

   Result;

end C93005b;
