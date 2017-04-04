-- C85009A.ADA

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
--     CHECK THAT PREDEFINED AND USER-DEFINED EXCEPTIONS CAN BE RENAMED
--     AND THAT HANDLERS REFERRING TO EITHER NAME ARE INVOKED WHEN THE
--     EXCEPTION IS RAISED, EVEN BY AN EXPLICIT 'RAISE' STATEMENT
--     REFERRING TO THE OTHER NAME.

-- HISTORY:
--     JET 03/24/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C85009a is

   My_Exception : exception;

   My_Exception2 : exception renames My_Exception;

   Constraint_Error2 : exception renames Constraint_Error;

   I : Integer := 1;

begin
   Test
     ("C85009A",
      "CHECK THAT PREDEFINED AND USER-DEFINED " &
      "EXCEPTIONS CAN BE RENAMED AND THAT HANDLERS " &
      "REFERRING TO EITHER NAME ARE INVOKED WHEN " &
      "THE EXCEPTION IS RAISED, EVEN BY AN EXPLICIT " &
      "'RAISE' STATEMENT REFERRING TO THE OTHER NAME");

   begin
      raise My_Exception;
      Failed ("MY_EXCEPTION NOT RAISED");
   exception
      when My_Exception2 =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED BY MY_EXCEPTION");
   end;

   begin
      raise My_Exception2;
      Failed ("MY_EXCEPTION2 NOT RAISED");
   exception
      when My_Exception =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED BY MY_EXCEPTION2");
   end;

   declare
      type Colors is (Red, Blue, Yellow);
      E : Colors := Red;
   begin
      E := Colors'Pred (E);
      if not Equal (Colors'Pos (E), Colors'Pos (E)) then
         Comment ("DON'T OPTIMIZE E");
      end if;
      Failed ("CONSTRAINT_ERROR NOT RAISED BY PRED(RED)");
   exception
      when Constraint_Error2 =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED BY PRED(RED)");
   end;

   begin
      raise Constraint_Error;
      Failed ("CONSTRAINT_ERROR NOT RAISED");
   exception
      when Constraint_Error2 =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED BY CONSTRAINT_ERROR");
   end;

   begin
      raise Constraint_Error2;
      Failed ("CONSTRAINT_ERROR2 NOT RAISED");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED BY CONSTRAINT_ERROR2");
   end;

   Result;
end C85009a;
