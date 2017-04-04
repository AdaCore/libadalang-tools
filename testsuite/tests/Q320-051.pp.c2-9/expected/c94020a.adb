-- C94020A.ADA

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
-- CHECK THAT THE CONDITIONS FOR TERMINATION ARE RECOGNIZED WHEN THE LAST
-- MISSING TASK TERMINATES DUE TO AN ABORT

-- JEAN-PIERRE ROSEN 08-MAR-1984 JBG 6/1/84 PWN 09/11/94 REMOVED PRAGMA
-- PRIORITY FOR ADA 9X.

with System; use System;
with Report; use Report;
procedure C94020a is

   task type T2 is
   end T2;

   task type T3 is
      entry E;
   end T3;

   task body T2 is
   begin
      Comment ("T2");
   end T2;

   task body T3 is
   begin
      Comment ("T3");
      select
         accept E;
      or
         terminate;
      end select;
      Failed ("T3 EXITED SELECT OR TERMINATE");
   end T3;

begin

   Test ("C94020A", "TEST OF TASK DEPENDENCES, TERMINATE, ABORT");

   declare
      task type T1 is
      end T1;

      V1 : T1;
      type A_T1 is access T1;

      task body T1 is
      begin
         abort T1;
         delay 0.0;          --SYNCHRONIZATION POINT
         Failed ("T1 NOT ABORTED");
      end T1;

   begin
      declare
         V2 : T2;
         A1 : A_T1;
      begin
         declare
            V3 : T3;
            task T4 is
            end T4;
            task body T4 is
               task T41 is
               end T41;
               task body T41 is
               begin
                  Comment ("T41");
                  abort T4;
                  delay 0.0;       --SYNCHRONIZATION POINT
                  Failed ("T41 NOT ABORTED");
               end T41;
            begin  --T4
               Comment ("T4");
            end T4;
         begin
            Comment ("BLOC 3");
         end;
         Comment ("BLOC 2");
         A1 := new T1;
      end;
      Comment ("BLOC 1");
   exception
      when others =>
         Failed ("SOME EXCEPTION RAISED");
   end;

   Result;

end C94020a;
