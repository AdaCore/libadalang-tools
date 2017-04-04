-- C94010A.ADA

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
-- CHECK THAT IF A GENERIC UNIT HAS A FORMAL LIMITED PRIVATE TYPE AND DECLARES
-- AN OBJECT OF THAT TYPE (OR HAS A SUBCOMPONENT OF THAT TYPE), AND IF THE UNIT
-- IS INSTANTIATED WITH A TASK TYPE OR AN OBJECT HAVING A SUBCOMPONENT OF A
-- TASK TYPE, THEN THE USUAL RULES APPLY TO THE INSTANTIATED UNIT, NAMELY:
--     A) IF THE GENERIC UNIT IS A SUBPROGRAM, CONTROL CANNOT LEAVE THE
--        SUBPROGRAM UNTIL THE TASK CREATED BY THE OBJECT DECLARATION IS
--        TERMINATED.

-- THIS TEST CONTAINS RACE CONDITIONS AND SHARED VARIABLES.

-- TBN  9/22/86
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

with Report; use Report;
with System; use System;
procedure C94010a is

   Global_Int : Integer := 0;
   My_Exception : exception;

   package P is
      type Lim_Pri_Task is limited private;
   private
      task type Lim_Pri_Task is
      end Lim_Pri_Task;
   end P;

   use P;

   task type Tt is
   end Tt;

   type Rec is record
      A : Integer := 1;
      B : Tt;
   end record;

   type Lim_Rec is record
      A : Integer := 1;
      B : Lim_Pri_Task;
   end record;

   package body P is
      task body Lim_Pri_Task is
      begin
         delay 30.0;
         Global_Int := Ident_Int (2);
      end Lim_Pri_Task;
   end P;

   task body Tt is
   begin
      delay 30.0;
      Global_Int := Ident_Int (1);
   end Tt;

   generic
      type T is limited private;
   procedure Proc (A : Integer);

   procedure Proc (A : Integer) is
      Obj_T : T;
   begin
      if A = Ident_Int (1) then
         raise My_Exception;
      end if;
   end Proc;

   generic
      type T is limited private;
   function Func (A : Integer) return Integer;

   function Func (A : Integer) return Integer is
      Obj_T : T;
   begin
      if A = Ident_Int (1) then
         raise My_Exception;
      end if;
      return 1;
   end Func;

begin
   Test
     ("C94010A",
      "CHECK TERMINATION RULES FOR INSTANTIATIONS OF " &
      "GENERIC SUBPROGRAM UNITS WHICH CREATE TASKS");

   -------------------------------------------------------------------
   declare
      procedure Proc1 is new Proc (Tt);
   begin
      Proc1 (0);
      if Global_Int = Ident_Int (0) then
         Failed ("TASK NOT DEPENDENT ON MASTER - 1");
         delay 35.0;
      end if;
   end;

   -------------------------------------------------------------------
   Global_Int := Ident_Int (0);

   declare
      procedure Proc2 is new Proc (Rec);
   begin
      Proc2 (1);
      Failed ("EXCEPTION WAS NOT RAISED - 2");
   exception
      when My_Exception =>
         if Global_Int = Ident_Int (0) then
            Failed ("TASK NOT DEPENDENT ON MASTER - 2");
            delay 35.0;
         end if;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 2");
   end;

   -------------------------------------------------------------------
   Global_Int := Ident_Int (0);

   declare
      procedure Proc3 is new Proc (Lim_Pri_Task);
   begin
      Proc3 (1);
      Failed ("EXCEPTION WAS NOT RAISED - 3");
   exception
      when My_Exception =>
         if Global_Int = Ident_Int (0) then
            Failed ("TASK NOT DEPENDENT ON MASTER - 3");
            delay 35.0;
         end if;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 3");
   end;

   -------------------------------------------------------------------
   Global_Int := Ident_Int (0);

   declare
      procedure Proc4 is new Proc (Lim_Rec);
   begin
      Proc4 (0);
      if Global_Int = Ident_Int (0) then
         Failed ("TASK NOT DEPENDENT ON MASTER - 4");
         delay 35.0;
      end if;
   end;

   -------------------------------------------------------------------
   Global_Int := Ident_Int (0);

   declare
      A : Integer;
      function Func1 is new Func (Tt);
   begin
      A := Func1 (1);
      Failed ("EXCEPTION NOT RAISED - 5");
   exception
      when My_Exception =>
         if Global_Int = Ident_Int (0) then
            Failed ("TASK NOT DEPENDENT ON MASTER - 5");
            delay 35.0;
         end if;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 5");
   end;

   -------------------------------------------------------------------
   Global_Int := Ident_Int (0);

   declare
      A : Integer;
      function Func2 is new Func (Rec);
   begin
      A := Func2 (0);
      if Global_Int = Ident_Int (0) then
         Failed ("TASK NOT DEPENDENT ON MASTER - 6");
         delay 35.0;
      end if;
   end;

   -------------------------------------------------------------------
   Global_Int := Ident_Int (0);

   declare
      A : Integer;
      function Func3 is new Func (Lim_Pri_Task);
   begin
      A := Func3 (0);
      if Global_Int = Ident_Int (0) then
         Failed ("TASK NOT DEPENDENT ON MASTER - 7");
         delay 35.0;
      end if;
   end;

   -------------------------------------------------------------------
   Global_Int := Ident_Int (0);

   declare
      A : Integer;
      function Func4 is new Func (Lim_Rec);
   begin
      A := Func4 (1);
      Failed ("EXCEPTION NOT RAISED - 8");
   exception
      when My_Exception =>
         if Global_Int = Ident_Int (0) then
            Failed ("TASK NOT DEPENDENT ON MASTER - 8");
         end if;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 8");
   end;

   -------------------------------------------------------------------

   Result;
end C94010a;
