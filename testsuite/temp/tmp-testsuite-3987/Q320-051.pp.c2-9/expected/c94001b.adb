-- C94001B.ADA

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
-- CHECK THAT A UNIT WITH DEPENDENT TASKS CREATED BY AN OBJECT
-- DECLARATION OF LIMITED PRIVATE TYPE IS NOT TERMINATED UNTIL ALL
-- DEPENDENT TASKS BECOME TERMINATED.
-- SUBTESTS ARE:
--   (A, B)  A SIMPLE TASK OBJECT, IN A BLOCK.
--   (C, D)  AN ARRAY OF TASK OBJECT, IN A FUNCTION.
--   (E, F)  AN ARRAY OF RECORD OF TASK OBJECT, IN A TASK BODY.

-- THIS TEST CONTAINS SHARED VARIABLES AND RACE CONDITIONS.

-- TBN  8/22/86
-- PWN 01/31/95  REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Report; use Report;
with System; use System;
procedure C94001b is

   package P is
      My_Exception : exception;
      Global : Integer;
      type Tt is limited private;
      procedure Call_Entry (A : Tt; B : Integer);
   private
      task type Tt is
         entry E (I : Integer);
      end Tt;
   end P;

   package body P is

      procedure Call_Entry (A : Tt; B : Integer) is
      begin
         A.E (B);
      end Call_Entry;

      task body Tt is
         Local : Integer;
      begin
         accept E (I : Integer) do
            Local := I;
         end E;
         delay 30.0;    -- SINCE THE PARENT UNIT HAS HIGHER
         -- PRIORITY AT THIS POINT, IT WILL
         -- RECEIVE CONTROL AND TERMINATE IF
         -- THE ERROR IS PRESENT.
         Global := Local;
      end Tt;
   end P;

   use P;

begin
   Test
     ("C94001B",
      "CHECK THAT A UNIT WITH DEPENDENT TASKS " &
      "CREATED BY AN OBJECT DECLARATION OF LIMITED " &
      "PRIVATE TYPE IS NOT TERMINATED UNTIL ALL " &
      "DEPENDENT TASKS BECOME TERMINATED");

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (A)

      T : Tt;

   begin -- (A)

      Call_Entry (T, Ident_Int (1));

   end; -- (A)

   if Global /= 1 then
      Failed ("DEPENDENT TASK NOT TERMINATED BEFORE " & "BLOCK EXIT - 1");
   end if;

   --------------------------------------------------

   Global := Ident_Int (0);

   begin -- (B)
      declare
         T : Tt;
      begin
         Call_Entry (T, Ident_Int (2));
         raise My_Exception;
      end;

      Failed ("MY_EXCEPTION WAS NOT RAISED - 2");
   exception
      when My_Exception =>
         if Global /= 2 then
            Failed
              ("DEPENDENT TASK NOT TERMINATED BEFORE " & "BLOCK EXIT - 2");
         end if;
      when others =>
         Failed ("UNEXPECTED EXCEPTION - 2");
   end; -- (B)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (C)

      I : Integer;

      function F return Integer is
         A : array (1 .. 1) of Tt;
      begin
         Call_Entry (A (1), Ident_Int (3));
         return 0;
      end F;

   begin -- (C)

      I := F;

      if Global /= 3 then
         Failed
           ("DEPENDENT TASK NOT TERMINATED BEFORE " & "FUNCTION EXIT - 3");
      end if;

   end; -- (C)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (D)

      I : Integer;

      function F return Integer is
         A : array (1 .. 1) of Tt;
      begin
         Call_Entry (A (1), Ident_Int (4));
         if Equal (3, 3) then
            raise My_Exception;
         end if;
         return 0;
      end F;

   begin -- (D)
      I := F;
      Failed ("MY_EXCEPTION WAS NOT RAISED - 4");
   exception
      when My_Exception =>
         if Global /= 4 then
            Failed
              ("DEPENDENT TASK NOT TERMINATED BEFORE " & "FUNCTION EXIT - 4");
         end if;
      when others =>
         Failed ("UNEXPECTED EXCEPTION - 4");
   end; -- (D)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (E)

      Loop_Count : Integer  := 0;
      Cut_Off    : constant := 60 * 60;     -- ONE HOUR DELAY.

      task Tsk is
         entry Ent;
      end Tsk;

      task body Tsk is
         type Rt is record
            T : Tt;
         end record;
         Ar : array (1 .. 1) of Rt;
      begin
         Call_Entry (Ar (1).T, Ident_Int (5));
      end Tsk;

   begin -- (E)

      while not Tsk'Terminated and Loop_Count < Cut_Off loop
         delay 1.0;
         Loop_Count := Loop_Count + 1;
      end loop;

      if Loop_Count >= Cut_Off then
         Failed ("DEPENDENT TASK NOT TERMINATED WITHIN ONE " & "HOUR - 5");
      elsif Global /= 5 then
         Failed ("DEPENDENT TASK NOT TERMINATED BEFORE " & "TASK EXIT - 5");
      end if;

   end; -- (E)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (F)

      Loop_Count : Integer  := 0;
      Cut_Off    : constant := 60 * 60;     -- ONE HOUR DELAY.

      task Tsk is
         entry Ent;
      end Tsk;

      task body Tsk is
         type Rt is record
            T : Tt;
         end record;
         Ar : array (1 .. 1) of Rt;
      begin
         Call_Entry (Ar (1).T, Ident_Int (6));
         if Equal (3, 3) then
            raise My_Exception;
         end if;
         Failed ("EXCEPTION WAS NOT RAISED - 6");
      end Tsk;

   begin -- (F)

      while not Tsk'Terminated and Loop_Count < Cut_Off loop
         delay 1.0;
         Loop_Count := Loop_Count + 1;
      end loop;

      if Loop_Count >= Cut_Off then
         Failed ("DEPENDENT TASK NOT TERMINATED WITHIN ONE " & "HOUR - 6");
      elsif Global /= 6 then
         Failed ("DEPENDENT TASK NOT TERMINATED BEFORE " & "TASK EXIT - 6");
      end if;

   end; -- (F)

   Result;
end C94001b;
