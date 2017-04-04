-- C93005A.ADA

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

-- JEAN-PIERRE ROSEN 3/9/84
-- JBG 06/01/84
-- JBG 05/23/85
-- EG  10/29/85  ELIMINATE THE USE OF NUMERIC_ERROR IN TEST.
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

with Report; use Report;
with System; use System;
procedure C93005a is

begin
   Test
     ("C93005A",
      "EXCEPTIONS RAISED IN A DECLARATIVE PART " & "CONTAINING TASKS");

   begin

      declare
         task type T1 is     -- CHECKS THAT T2 TERMINATES.
         end T1;

         type At1 is access T1;

         task T2 is          -- WILL NEVER BE ACTIVATED.
            entry E;
         end T2;

         package Raise_It is
         end Raise_It;

         task body T2 is
         begin
            Failed ("T2 ACTIVATED");
            -- IN CASE OF FAILURE
            loop
               select
                  accept E;
               or
                  terminate;
               end select;
            end loop;
         end T2;

         task body T1 is
         begin
            declare  -- THIS BLOCK TO CHECK THAT T3 TERMINATES.
               task T3 is
               end T3;

               task body T3 is
               begin
                  T2.E;
                  Failed ("RENDEZVOUS COMPLETED WITHOUT " & "ERROR - T3");
               exception
                  when Tasking_Error =>
                     null;
                  when others =>
                     Failed ("ABNORMAL EXCEPTION - T3");
               end T3;
            begin
               null;
            end;

            T2.E;    --T2 IS NOW TERMINATED

            Failed ("RENDEZVOUS COMPLETED WITHOUT ERROR - T1");

         exception
            when Tasking_Error =>
               null;
            when others =>
               Failed ("ABNORMAL EXCEPTION - T1");
         end T1;

         package body Raise_It is
            Pt1 : At1      := new T1;
            I   : Positive := Ident_Int (0); -- RAISE
         -- CONSTRAINT_ERROR.
         begin
            if I /= Ident_Int (2) or I = Ident_Int (1) + 1 then
               Failed ("PACKAGE DIDN'T RAISE EXCEPTION");
            end if;
         end Raise_It;

      begin     -- CAN'T LEAVE BLOCK UNTIL T1, T2, AND T3 ARE TERM.
         Failed ("EXCEPTION NOT RAISED");
      end;

   exception
      when Constraint_Error =>
         null;
      when Tasking_Error =>
         Failed ("TASKING_ERROR IN MAIN PROGRAM");
      when others =>
         Failed ("ABNORMAL EXCEPTION IN MAIN-1");
   end;

   Result;

end C93005a;
