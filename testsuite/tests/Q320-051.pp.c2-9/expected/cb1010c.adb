-- CB1010C.ADA

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
-- CHECK THAT STORAGE_ERROR IS RAISED WHEN STORAGE FOR A DECLARATIVE ITEM IS
-- INSUFFICIENT.

-- JRK 8/30/85

with Report; use Report;

procedure Cb1010c is

   N : Integer := Ident_Int (1_000);
   M : Integer := Ident_Int (0);

   procedure Overflow_Stack is
   begin
      N := N + M;
      declare
         A : array (1 .. N) of Integer;
      begin
         A (N) := M;
         if N > M then  -- ALWAYS TRUE.
            Overflow_Stack;
         end if;
         M := A (N);    -- TO PREVENT TAIL RECURSION OPTIMIZATION.
      end;
   end Overflow_Stack;

begin
   Test
     ("CB1010C",
      "CHECK THAT STORAGE_ERROR IS RAISED WHEN " &
      "STORAGE FOR A DECLARATIVE ITEM IS INSUFFICIENT");

   begin

      Overflow_Stack;
      Failed ("EXCEPTION NOT RAISED BY STACK OVERFLOW");

   exception
      when Storage_Error =>
         if N /= 1_000 or M /= 0 then
            Failed ("VALUES OF VARIABLES N OR M WERE ALTERED");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED BY STACK OVERFLOW");
   end;

   Result;
end Cb1010c;
