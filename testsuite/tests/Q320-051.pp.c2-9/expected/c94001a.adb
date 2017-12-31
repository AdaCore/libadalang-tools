-- C94001A.ADA

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
-- CHECK THAT A UNIT WITH DEPENDENT TASKS CREATED BY OBJECT
--   DECLARATIONS IS NOT TERMINATED UNTIL ALL DEPENDENT TASKS BECOME
--   TERMINATED.
-- SUBTESTS ARE:
--   (A, B)  A SIMPLE TASK OBJECT, IN A BLOCK.
--   (C, D)  AN ARRAY OF TASK OBJECT, IN A FUNCTION.
--   (E, F)  AN ARRAY OF RECORD OF TASK OBJECT, IN A TASK BODY.

-- THIS TEST CONTAINS SHARED VARIABLES AND RACE CONDITIONS.

-- JRK 10/2/81
-- SPS 11/21/82
-- JRK 11/29/82
-- TBN 8/22/86 REVISED; ADDED CASES THAT EXIT BY RAISING AN
--                  EXCEPTION.
-- PWN 01/31/95 REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Report; use Report;
with System; use System;
procedure C94001a is

   My_Exception : exception;
   Global : Integer;

   task type Tt is
      entry E (I : Integer);
   end Tt;

   task body Tt is
      Local : Integer;
   begin
      accept E (I : Integer) do
         Local := I;
      end E;
      delay 30.0;    -- SINCE THE PARENT UNIT HAS HIGHER PRIORITY
      -- AT THIS POINT, IT WILL RECEIVE CONTROL AND TERMINATE IF THE ERROR IS
      -- PRESENT.
      Global := Local;
   end Tt;

begin
   Test
     ("C94001A",
      "CHECK THAT A UNIT WITH DEPENDENT TASKS " &
      "CREATED BY OBJECT DECLARATIONS IS NOT " &
      "TERMINATED UNTIL ALL DEPENDENT TASKS " & "BECOME TERMINATED");

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (A)

      T : Tt;

   begin -- (A)

      T.E (Ident_Int (1));

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
         T.E (Ident_Int (1));
         raise My_Exception;
      end;

      Failed ("MY_EXCEPTION WAS NOT RAISED - 2");
   exception
      when My_Exception =>
         if Global /= 1 then
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
         A (1).E (Ident_Int (2));
         return 0;
      end F;

   begin -- (C)

      I := F;

      if Global /= 2 then
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
         A (1).E (Ident_Int (2));
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
         if Global /= 2 then
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
         Ar (1).T.E (Ident_Int (3));
      end Tsk;

   begin -- (E)

      while not Tsk'Terminated and Loop_Count < Cut_Off loop
         delay 1.0;
         Loop_Count := Loop_Count + 1;
      end loop;

      if Loop_Count >= Cut_Off then
         Failed ("DEPENDENT TASK NOT TERMINATED WITHIN ONE " & "HOUR - 5");
      elsif Global /= 3 then
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
         Ar (1).T.E (Ident_Int (3));
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
      elsif Global /= 3 then
         Failed ("DEPENDENT TASK NOT TERMINATED BEFORE " & "TASK EXIT - 6");
      end if;

   end; -- (F)

   --------------------------------------------------

   Result;
end C94001a;
