-- CB1010D.ADA

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
-- CHECK THAT STORAGE_ERROR IS RAISED WHEN STORAGE FOR THE EXECUTION OF A
-- SUBPROGRAM IS INSUFFICIENT.

-- PNH 8/26/85
-- JRK 8/30/85

with Report; use Report;

procedure Cb1010d is

   N : Integer := Ident_Int (1);
   M : Integer := Ident_Int (0);

   procedure Overflow_Stack is
   begin
      N := N + M;
      if N > M then       -- ALWAYS TRUE.
         Overflow_Stack;
      end if;
      N := N - M;         -- TO PREVENT TAIL RECURSION OPTIMIZATION.
   end Overflow_Stack;

begin
   Test
     ("CB1010D",
      "CHECK THAT STORAGE_ERROR IS RAISED WHEN " &
      "STORAGE FOR THE EXECUTION OF A SUBPROGRAM " & "IS INSUFFICIENT");

   -- CHECK HANDLING OF STORAGE_ERROR IN MAIN PROGRAM.

   begin
      Overflow_Stack;
      Failed ("EXCEPTION NOT RAISED BY STACK OVERFLOW - 1");
   exception
      when Storage_Error =>
         if N /= 1 then
            Failed ("VALUE OF VARIABLE N ALTERED - 1");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED BY STACK OVERFLOW - 1");
   end;

   -- CHECK HANDLING OF STORAGE_ERROR IN SUBPROGRAM.

   declare

      procedure P is
      begin
         Overflow_Stack;
         Failed ("EXCEPTION NOT RAISED BY STACK OVERFLOW - 2");
      exception
         when Storage_Error =>
            if N /= 1 then
               Failed ("VALUE OF VARIABLE N ALTERED - 2");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED BY STACK " & "OVERFLOW - 2");
      end P;

   begin

      N := Ident_Int (1);
      P;

   end;

   Result;
end Cb1010d;
