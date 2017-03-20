-- C94001C.ADA

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
-- CHECK THAT A UNIT WITH INDIRECT DEPENDENT TASKS CREATED BY OBJECT
-- DECLARATIONS IS NOT TERMINATED UNTIL ALL INDIRECT DEPENDENT TASKS
-- BECOME TERMINATED.
-- SUBTESTS ARE:
--   (A, B)  A BLOCK CONTAINING A SIMPLE TASK OBJECT, IN A BLOCK.
--   (C, D)  A FUNCTION CONTAINING AN ARRAY OF TASK OBJECT, IN A
--           FUNCTION.
--   (E, F)  A TASK CONTAINING AN ARRAY OF RECORD OF TASK OBJECT,
--           IN A TASK BODY.
--   CASES (B, D, F) EXIT BY RAISING AN EXCEPTION.

-- THIS TEST CONTAINS SHARED VARIABLES AND RACE CONDITIONS.

-- TBN  8/25/86
-- PWN 01/31/95  REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Report; use Report;
with System; use System;
procedure C94001c is

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
      -- AT THIS POINT, IT WILL RECEIVE CONTROL AND
      -- TERMINATE IF THE ERROR IS PRESENT.
      Global := Local;
   end Tt;

begin
   Test
     ("C94001C",
      "CHECK THAT A UNIT WITH INDIRECT DEPENDENT " &
      "TASKS CREATED BY OBJECT DECLARATIONS IS NOT " &
      "TERMINATED UNTIL ALL INDIRECT DEPENDENT TASKS " &
      "BECOME TERMINATED");

   --------------------------------------------------
   Global := Ident_Int (0);

   begin -- (A)

      declare
         T : Tt;
      begin
         T.E (Ident_Int (1));
      end;

   end; -- (A)

   if Global /= 1 then
      Failed ("DEPENDENT TASK NOT TERMINATED BEFORE " & "BLOCK EXIT - 1");
   end if;

   --------------------------------------------------

   begin -- (B)
      Global := Ident_Int (0);

      begin
         declare
            T : Tt;
         begin
            T.E (Ident_Int (2));
            raise My_Exception;
         end;
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

      Obj_Int : Integer;

      function F1 return Integer is
         I : Integer;

         function F2 return Integer is
            A : array (1 .. 1) of Tt;
         begin
            A (1).E (Ident_Int (3));
            return 0;
         end F2;
      begin
         I := F2;
         return (0);
      end F1;

   begin -- (C)
      Obj_Int := F1;
      if Global /= 3 then
         Failed
           ("DEPENDENT TASK NOT TERMINATED BEFORE " & "FUNCTION EXIT - 3");
      end if;
   end; -- (C)

   --------------------------------------------------

   declare -- (D)

      Obj_Int : Integer;

      function F1 return Integer is
         I : Integer;

         function F2 return Integer is
            A : array (1 .. 1) of Tt;
         begin
            A (1).E (Ident_Int (4));
            if Equal (3, 3) then
               raise My_Exception;
            end if;
            return 0;
         end F2;
      begin
         I := F2;
         return (0);
      end F1;

   begin -- (D)
      Global  := Ident_Int (0);
      Obj_Int := F1;
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
      Delay_Count : Integer := 0;
      task Out_Tsk;

      task body Out_Tsk is

         task Tsk is
            entry Ent;
         end Tsk;

         task body Tsk is
            type Rt is record
               T : Tt;
            end record;
            Ar : array (1 .. 1) of Rt;
         begin
            Ar (1).T.E (Ident_Int (5));
         end Tsk;

      begin
         null;
      end Out_Tsk;

   begin -- (E)
      while not (Out_Tsk'Terminated) and Delay_Count < 60 loop
         delay 1.0;
         Delay_Count := Delay_Count + 1;
      end loop;
      if Delay_Count = 60 then
         Failed ("OUT_TSK HAS NOT TERMINATED - 5");
      elsif Global /= 5 then
         Failed ("DEPENDENT TASK NOT TERMINATED BEFORE " & "BLOCK EXIT - 5");
      end if;
   end; -- (E)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare
      Delay_Count : Integer := 0;

      task Out_Tsk;

      task body Out_Tsk is

         task Tsk is
            entry Ent;
         end Tsk;

         task body Tsk is
            type Rt is record
               T : Tt;
            end record;
            Ar : array (1 .. 1) of Rt;
         begin
            Ar (1).T.E (Ident_Int (6));
            raise My_Exception;
         end Tsk;

      begin
         raise My_Exception;
      end Out_Tsk;

   begin
      while not (Out_Tsk'Terminated) and Delay_Count < 60 loop
         delay 1.0;
         Delay_Count := Delay_Count + 1;
      end loop;
      if Delay_Count = 60 then
         Failed ("OUT_TSK HAS NOT TERMINATED - 6");
      elsif Global /= 6 then
         Failed ("DEPENDENT TASK NOT TERMINATED BEFORE " & "BLOCK EXIT - 6");
      end if;
   end;

   Result;
end C94001c;
