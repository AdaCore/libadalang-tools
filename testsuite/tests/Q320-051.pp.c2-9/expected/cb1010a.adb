-- CB1010A.ADA

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
-- CHECK THAT STORAGE_ERROR IS RAISED WHEN STORAGE ALLOCATED TO A TASK IS
-- EXCEEDED.

-- PNH 8/26/85
-- JRK 8/30/85

with Report; use Report;

procedure Cb1010a is

   N : Integer := Ident_Int (1);
   M : Integer := Ident_Int (0);

   procedure Overflow_Stack is
      A : array (1 .. 1_000) of Integer;
   begin
      N     := N + M;
      A (N) := M;
      if N > M then  -- ALWAYS TRUE.
         Overflow_Stack;
      end if;
      M := A (N);    -- TO PREVENT TAIL RECURSION OPTIMIZATION.
   end Overflow_Stack;

begin
   Test
     ("CB1010A",
      "CHECK THAT STORAGE_ERROR IS RAISED WHEN " &
      "STORAGE ALLOCATED TO A TASK IS EXCEEDED");

   --------------------------------------------------

   Comment
     ("CHECK TASKS THAT DO NOT HANDLE STORAGE_ERROR " & "PRIOR TO RENDEZVOUS");

   declare

      task T1 is
         entry E1;
      end T1;

      task body T1 is
      begin
         Overflow_Stack;
         Failed ("TASK T1 NOT TERMINATED BY STACK OVERFLOW");
      end T1;

   begin

      T1.E1;
      Failed ("NO EXCEPTION RAISED BY ENTRY CALL T1.E1");

   exception
      when Tasking_Error =>
         if N /= 1 or M /= 0 then
            Failed ("VALUES OF VARIABLES N OR M ALTERED - 1");
         end if;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED BY CALL OF ENTRY E1 " &
            "OF TERMINATED TASK T1");
   end;

   --------------------------------------------------

   Comment
     ("CHECK TASKS THAT DO HANDLE STORAGE_ERROR PRIOR TO " & "RENDEZVOUS");

   N := Ident_Int (1);
   M := Ident_Int (0);

   declare

      task T2 is
         entry E2;
      end T2;

      task body T2 is
      begin
         Overflow_Stack;
         Failed ("EXCEPTION NOT RAISED BY STACK OVERFLOW IN " & "TASK T2");
      exception
         when Storage_Error =>
            accept E2;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED IN TASK T2 BY " & "STACK OVERFLOW");
      end T2;

   begin

      T2.E2;
      if N /= 1 or M /= 0 then
         Failed ("VALUES OF VARIABLES N OR M ALTERED - 2");
      end if;

   exception
      when others =>
         Failed ("EXCEPTION RAISED BY ENTRY CALL T2.E2");
         abort T2;
   end;

   --------------------------------------------------

   Comment
     ("CHECK TASKS THAT DO NOT HANDLE STORAGE_ERROR " & "DURING RENDEZVOUS");

   N := Ident_Int (1);
   M := Ident_Int (0);

   declare

      task T3 is
         entry E3a;
         entry E3b;
      end T3;

      task body T3 is
      begin
         accept E3a do
            Overflow_Stack;
            Failed
              ("EXCEPTION NOT RAISED IN ACCEPT E3A BY " & "STACK OVERFLOW");
         end E3a;
         Failed ("EXCEPTION NOT PROPOGATED CORRECTLY IN TASK T3");
      exception
         when Storage_Error =>
            accept E3b;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED IN TASK T3 BY " & "STACK OVERFLOW");
      end T3;

   begin

      T3.E3a;
      Failed ("NO EXCEPTION RAISED BY ENTRY CALL T3.E3A");

   exception
      when Storage_Error =>
         T3.E3b;
         if N /= 1 or M /= 0 then
            Failed ("VALUES OF VARIABLES N OR M ALTERED - 3");
         end if;
      when Tasking_Error =>
         Failed
           ("TASKING_ERROR RAISED BY ENTRY CALL T3.E3A " &
            "INSTEAD OF STORAGE_ERROR");
         abort T3;
      when others =>
         Failed ("WRONG EXCEPTION RAISED BY ENTRY CALL T3.E3A");
         abort T3;
   end;

   --------------------------------------------------

   Result;
end Cb1010a;
