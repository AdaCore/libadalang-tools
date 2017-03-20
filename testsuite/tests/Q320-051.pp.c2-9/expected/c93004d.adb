-- C93004D.ADA

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
-- CHECK THAT WHEN AN EXCEPTION IS RAISED DURING THE ACTIVATION OF A
-- TASK, OTHER TASKS ARE UNAFFECTED.

-- THIS TEST CHECKS THE CASE IN WHICH SOME OF THE OTHER TASKS ARE
-- PERHAPS ACTIVATED BEFORE THE EXCEPTION OCCURS AND SOME TASKS ARE
-- PERHAPS ACTIVATED AFTER.

-- THE ENCLOSING BLOCK RECEIVES TASKING_ERROR.

-- CHECK THAT TASKS WAITING FOR ENTRIES OF SUCH TASKS RECEIVE
-- TASKING_ERROR.

-- R. WILLIAMS 8/6/86
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

with System; use System;
with Report; use Report;
procedure C93004d is

begin
   Test
     ("C93004D",
      "CHECK THAT WHEN AN EXCEPTION IS RAISED " &
      "DURING ACTIVATION OF A TASK, OTHER TASKS " &
      "ARE NOT AFFECTED. IN THIS TEST, SOME OF THE " &
      "TASKS ARE PERHAPS ACTIVATED BEFORE THE " &
      "EXCEPTION OCCURS AND SOME PERHAPS AFTER");

   declare

      task T0 is
         entry E;
      end T0;

      task type T1 is
      end T1;

      task type T2 is
         entry E;
      end T2;

      Arr_T2 : array (Integer range 1 .. 4) of T2;

      type At1 is access T1;

      task body T0 is
      begin
         accept E;
      end T0;

      package Start_T1 is      -- THIS PACKAGE TO AVOID ACCESS
      end Start_T1;            -- BEFORE ELABORATION ON T1.

      task body T1 is
      begin
         declare   -- THIS BLOCK TO CHECK THAT T1BIS TERMINATES.
            task T1bis is
            end T1bis;

            task body T1bis is
            begin
               Arr_T2 (Ident_Int (2)).E;
               Failed ("RENDEZVOUS COMPLETED - T3");
            exception
               when Tasking_Error =>
                  null;
               when others =>
                  Failed ("ABNORMAL EXCEPTION - T3");
            end T1bis;
         begin
            null;
         end;

         Arr_T2 (Ident_Int (2)).E;   -- ARR_T2(2) IS NOW
         -- TERMINATED.

         Failed ("RENDEZVOUS COMPLETED WITHOUT ERROR - T1");

      exception
         when Tasking_Error =>
            null;
         when others =>
            Failed ("ABNORMAL EXCEPTION - T1");
      end T1;

      package body Start_T1 is
         V_At1 : At1 := new T1;
      end Start_T1;

      task body T2 is
         I : Positive := Ident_Int (0); -- RAISE CONSTRAINT_ERROR.
      begin
         if I /= Ident_Int (2) or I = Ident_Int (1) + 1 then
            Failed ("T2 ACTIVATED OK");
         end if;
      end T2;

      task T3 is
         entry E;
      end T3;

      task body T3 is
      begin     -- T3 MUST BE ACTIVATED OK.
         accept E;
      end T3;

   begin -- T0, ARR_T2 (1 .. 4), T3 ACTIVATED HERE.

      Failed ("TASKING_ERROR NOT RAISED IN MAIN");
      T3.E;          -- CLEAN UP.
      T0.E;
   exception
      when Tasking_Error =>
         begin
            T3.E;
            T0.E;
         exception
            when Tasking_Error =>
               Failed ("T0 OR T3 NOT ACTIVATED");
         end;
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED IN MAIN");
      when others =>
         Failed ("ABNORMAL EXCEPTION IN MAIN-2");
   end;

   Result;
end C93004d;
