-- C94002A.ADA

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
-- CHECK THAT A UNIT WITH DEPENDENT TASKS CREATED BY (LOCAL)
--   ALLOCATORS DOES NOT TERMINATE UNTIL ALL DEPENDENT TASKS ARE
--   TERMINATED.
-- SUBTESTS ARE:
--   (A, B)  A SIMPLE TASK ALLOCATOR, IN A BLOCK.
--   (C, D)  A RECORD OF TASK ALLOCATOR, IN A FUNCTION.
--   (E, F)  A RECORD OF ARRAY OF TASK ALLOCATOR, IN A TASK BODY.

-- THIS TEST CONTAINS SHARED VARIABLES AND RACE CONDITIONS.

-- JRK 10/2/81
-- SPS 11/2/82
-- SPS 11/21/82
-- JRK 11/29/82
-- TBN 8/25/86 REDUCED DELAYS; ADDED LIMITED PRIVATE TYPES;
--                  INCLUDED EXITS BY RAISING AN EXCEPTION.
-- PWN 01/31/95 REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Report; use Report;
with System; use System;
procedure C94002a is

   package P is
      My_Exception : exception;
      Global : Integer;
      task type T1 is
         entry E (I : Integer);
      end T1;
      type T2 is limited private;
      procedure Call_Entry (A : T2; B : Integer);
   private
      task type T2 is
         entry E (I : Integer);
      end T2;
   end P;

   package body P is
      task body T1 is
         Local : Integer;
      begin
         accept E (I : Integer) do
            Local := I;
         end E;
         delay 30.0;    -- SINCE THE PARENT UNIT HAS HIGHER
         -- PRIORITY AT THIS POINT, IT WILL RECEIVE CONTROL AND TERMINATE IF
         -- THE ERROR IS PRESENT.
         Global := Local;
      end T1;

      task body T2 is
         Local : Integer;
      begin
         accept E (I : Integer) do
            Local := I;
         end E;
         delay 30.0;
         Global := Local;
      end T2;

      procedure Call_Entry (A : T2; B : Integer) is
      begin
         A.E (B);
      end Call_Entry;
   end P;

   use P;

begin
   Test
     ("C94002A",
      "CHECK THAT A UNIT WITH DEPENDENT TASKS " &
      "CREATED BY (LOCAL) ALLOCATORS DOES NOT " &
      "TERMINATE UNTIL ALL DEPENDENT TASKS " & "ARE TERMINATED");

   --------------------------------------------------
   Global := Ident_Int (0);
   begin -- (A)
      declare
         type A_T is access T1;
         A : A_T;
      begin
         if Equal (3, 3) then
            A := new T1;
            A.all.E (Ident_Int (1));
            raise My_Exception;
         end if;
      end;

      Failed ("MY_EXCEPTION WAS NOT RAISED - 1");
   exception
      when My_Exception =>
         if Global /= 1 then
            Failed
              ("DEPENDENT TASK NOT TERMINATED BEFORE " & "BLOCK EXIT - 1");
         end if;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 1");
   end; -- (A)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (B)
      type A_T is access T2;
      A : A_T;
   begin -- (B)
      if Equal (3, 3) then
         A := new T2;
         Call_Entry (A.all, Ident_Int (2));
      end if;
   end; -- (B)

   if Global /= 2 then
      Failed ("DEPENDENT TASK NOT TERMINATED BEFORE " & "BLOCK EXIT - 2");
   end if;

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (C)
      I : Integer;

      function F return Integer is
         type Rt;
         type Art is access Rt;
         type Rt is record
            A : Art;
            T : T1;
         end record;
         List : Art;
         Temp : Art;
      begin
         for I in 1 .. Ident_Int (1) loop
            Temp   := new Rt;
            Temp.A := List;
            List   := Temp;
            List.T.E (Ident_Int (3));
         end loop;
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
         type Rt;
         type Art is access Rt;
         type Rt is record
            A : Art;
            T : T2;
         end record;
         List : Art;
         Temp : Art;
      begin
         for I in 1 .. Ident_Int (1) loop
            Temp   := new Rt;
            Temp.A := List;
            List   := Temp;
            Call_Entry (List.T, Ident_Int (4));
            if Equal (3, 3) then
               raise My_Exception;
            end if;
         end loop;
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
         Failed ("UNEXPECTED EXCEPTION RAISED - 4");
   end; -- (D)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (E)

      Loop_Count : Integer  := 0;
      Cut_Off    : constant := 5 * 60;     -- FIVE MINUTE DELAY.

      task Tsk is
         entry Ent;
      end Tsk;

      task body Tsk is
         type Arr is array (1 .. 1) of T1;
         type Rat;
         type Arat is access Rat;
         type Rat is record
            A : Arat;
            T : Arr;
         end record;
         List : Arat;
         Temp : Arat;
      begin
         for I in 1 .. Ident_Int (1) loop
            Temp   := new Rat;
            Temp.A := List;
            List   := Temp;
            List.T (1).E (Ident_Int (5));
            if Equal (3, 3) then
               raise My_Exception;
            end if;
         end loop;
      end Tsk;

   begin -- (E)

      while not Tsk'Terminated and Loop_Count < Cut_Off loop
         delay 1.0;
         Loop_Count := Loop_Count + 1;
      end loop;

      if Loop_Count >= Cut_Off then
         Failed ("DEPENDENT TASK NOT TERMINATED WITHIN FIVE " & "MINUTES - 5");
      end if;

      if Global /= 5 then
         Failed ("DEPENDENT TASK NOT TERMINATED BEFORE " & "TASK EXIT - 5");
      end if;

   end; -- (E)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (F)

      Loop_Count : Integer  := 0;
      Cut_Off    : constant := 5 * 60;     -- FIVE MINUTE DELAY.

      task Tsk is
         entry Ent;
      end Tsk;

      task body Tsk is
         type Arr is array (1 .. 1) of T2;
         type Rat;
         type Arat is access Rat;
         type Rat is record
            A : Arat;
            T : Arr;
         end record;
         List : Arat;
         Temp : Arat;
      begin
         for I in 1 .. Ident_Int (1) loop
            Temp   := new Rat;
            Temp.A := List;
            List   := Temp;
            Call_Entry (List.T (1), Ident_Int (6));
         end loop;
      end Tsk;

   begin -- (F)

      while not Tsk'Terminated and Loop_Count < Cut_Off loop
         delay 1.0;
         Loop_Count := Loop_Count + 1;
      end loop;

      if Loop_Count >= Cut_Off then
         Failed ("DEPENDENT TASK NOT TERMINATED WITHIN FIVE " & "MINUTES - 6");
      end if;

      if Global /= 6 then
         Failed ("DEPENDENT TASK NOT TERMINATED BEFORE " & "TASK EXIT - 6");
      end if;

   end; -- (F)

   Result;
end C94002a;
