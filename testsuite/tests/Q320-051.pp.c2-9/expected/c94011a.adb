-- C94011A.ADA

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
-- CHECK THAT IF A FORMAL ACCESS TYPE OF A GENERIC UNIT DESIGNATES A FORMAL
-- LIMITED PRIVATE TYPE, THEN WHEN THE UNIT IS INSTANTIATED WITH A TASK
-- TYPE OR A TYPE HAVING A SUBCOMPONENT OF A TASK TYPE, THE MASTER FOR ANY
-- TASKS ALLOCATED WITHIN THE INSTANTIATED UNIT IS DETERMINED BY THE ACTUAL
-- PARAMETER.

-- TBN  9/22/86

with Report; use Report;
procedure C94011a is

   Global_Int : Integer := 0;
   My_Exception : exception;

   package P is
      type Lim_Pri_Task is limited private;
      procedure E (T : Lim_Pri_Task);
   private
      task type Lim_Pri_Task is
         entry E;
      end Lim_Pri_Task;
   end P;

   use P;

   task type Tt is
      entry E;
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
         accept E;
         Global_Int := Ident_Int (2);
      end Lim_Pri_Task;

      procedure E (T : Lim_Pri_Task) is
      begin
         T.E;
      end E;
   end P;

   task body Tt is
   begin
      accept E;
      Global_Int := Ident_Int (1);
   end Tt;

   generic
      type T is limited private;
      type Acc_T is access T;
   procedure Proc (A : out Acc_T);

   procedure Proc (A : out Acc_T) is
   begin
      A := new T;
   end Proc;

   generic
      type T is limited private;
      type Acc_T is access T;
   function Func return Acc_T;

   function Func return Acc_T is
   begin
      return new T;
   end Func;

   generic
      type T is limited private;
      type Acc_T is access T;
   package Pac is
      Ptr_T : Acc_T := new T;
   end Pac;

begin
   Test
     ("C94011A",
      "CHECK THAT IF A FORMAL ACCESS TYPE OF A " &
      "GENERIC UNIT DESIGNATES A FORMAL LIMITED " &
      "PRIVATE TYPE, THEN WHEN THE UNIT IS " &
      "INSTANTIATED, THE MASTER FOR ANY TASKS " &
      "ALLOCATED WITHIN THE INSTANTIATED UNIT IS " &
      "DETERMINED BY THE ACTUAL PARAMETER");

   -------------------------------------------------------------------
   declare
      type Acc_Tt is access Tt;
      Acc1 : Acc_Tt;
      procedure Proc1 is new Proc (Tt, Acc_Tt);
   begin
      Proc1 (Acc1);
      Acc1.E;
   exception
      when others =>
         Failed ("TASK DEPENDENT ON WRONG MASTER - 1");
   end;
   if Global_Int = Ident_Int (0) then
      Failed ("TASK NOT DEPENDENT ON MASTER - 1");
   end if;

   -------------------------------------------------------------------
   begin
      Global_Int := Ident_Int (0);
      declare
         type Acc_Rec is access Rec;
         A : Acc_Rec;
         function Func1 is new Func (Rec, Acc_Rec);
      begin
         A := Func1;
         A.B.E;
         raise My_Exception;
      exception
         when My_Exception =>
            raise My_Exception;
         when others =>
            Failed ("TASK DEPENDENT ON WRONG MASTER - 2");
      end;
      Failed ("MY_EXCEPTION NOT RAISED - 2");
   exception
      when My_Exception =>
         if Global_Int = Ident_Int (0) then
            Failed ("TASK NOT DEPENDENT ON MASTER - 2");
         end if;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 2");
   end;

   -------------------------------------------------------------------
   Global_Int := Ident_Int (0);

   begin
      declare
         type Acc_Lim_Tt is access Lim_Pri_Task;
      begin
         declare
            A : Acc_Lim_Tt;
            function Func2 is new Func (Lim_Pri_Task, Acc_Lim_Tt);
         begin
            A := Func2;
            E (A.all);
         end;
      exception
         when others =>
            Failed ("TASK DEPENDENT ON WRONG MASTER - 3");
      end;
      if Global_Int = Ident_Int (0) then
         Failed ("TASK NOT DEPENDENT ON MASTER - 3");
      end if;
   end;

   -------------------------------------------------------------------
   Global_Int := Ident_Int (0);

   begin
      declare
         type Acc_Lim_Rec is access Lim_Rec;
      begin
         declare
            Acc2 : Acc_Lim_Rec;
            procedure Proc2 is new Proc (Lim_Rec, Acc_Lim_Rec);
         begin
            Proc2 (Acc2);
            E (Acc2.B);
         end;
         raise My_Exception;
      exception
         when My_Exception =>
            raise My_Exception;
         when others =>
            Failed ("TASK DEPENDENT ON WRONG MASTER - 4");
      end;
      Failed ("MY_EXCEPTION NOT RAISED - 4");
   exception
      when My_Exception =>
         if Global_Int = Ident_Int (0) then
            Failed ("TASK NOT DEPENDENT ON MASTER - 4");
         end if;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 4");
   end;

   -------------------------------------------------------------------
   begin
      Global_Int := Ident_Int (0);

      declare
         type Acc_Tt is access Tt;
         package Pac1 is new Pac (Tt, Acc_Tt);
         use Pac1;
      begin
         Ptr_T.E;
         raise My_Exception;
      exception
         when My_Exception =>
            raise My_Exception;
         when others =>
            Failed ("TASK DEPENDENT ON WRONG MASTER - 5");
      end;
      Failed ("MY_EXCEPTION NOT RAISED - 5");
   exception
      when My_Exception =>
         if Global_Int = Ident_Int (0) then
            Failed ("TASK NOT DEPENDENT ON MASTER - 5");
         end if;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 5");
   end;

   -------------------------------------------------------------------
   Global_Int := Ident_Int (0);

   declare
      type Acc_Lim_Rec is access Lim_Rec;
   begin
      declare
         package Pac2 is new Pac (Lim_Rec, Acc_Lim_Rec);
         use Pac2;
      begin
         E (Ptr_T.B);
      end;
   exception
      when others =>
         Failed ("TASK DEPENDENT ON WRONG MASTER - 6");
   end;
   if Global_Int = Ident_Int (0) then
      Failed ("TASK NOT DEPENDENT ON MASTER - 6");
   end if;

   -------------------------------------------------------------------

   Result;
end C94011a;
