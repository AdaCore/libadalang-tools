-- C94002G.ADA

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
--     CHECK THAT A NON-MASTER UNIT, WHICH ALLOCATES TASKS OF A GLOBAL
--     ACCESS TYPE, MUST TERMINATE WITHOUT WAITING FOR THE ALLOCATED
--     TASKS TO TERMINATE IF AN EXCEPTION IS RAISED BUT NOT HANDLED IN
--     THE NON-MASTER UNIT.

--     SUBTESTS ARE:
--        (A)  A SIMPLE TASK ALLOCATOR, IN A BLOCK.
--        (B)  A RECORD OF TASK ALLOCATOR, IN A SUBPROGRAM.
--        (C)  A RECORD OF ARRAY OF TASK ALLOCATOR, IN A TASK BODY, NOT
--             DURING RENDEZVOUS.
--        (D)  A LIMITED PRIVATE TASK ALLOCATOR, IN A TASK BODY, DURING
--             RENDEZVOUS.

-- HISTORY:
--     TBN 01/20/86  CREATED ORIGINAL TEST.
--      JRK 05/01/86  IMPROVED ERROR RECOVERY.  FIXED EXCEPTION
--                    HANDLING.  ADDED CASE (D).
--      BCB 09/24/87  ADDED A RETURN STATEMENT TO THE HANDLER FOR OTHERS
--                    IN FUNCTION F, CASE B.
--      PWN 09/11/94  REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Report; use Report;
with System; use System;
procedure C94002g is

   My_Exception : exception;

   task type Tt is
      entry E;
   end Tt;

   task body Tt is
   begin
      accept E;
      accept E;
   end Tt;

begin
   Test
     ("C94002G",
      "CHECK THAT A NON-MASTER UNIT, WHICH ALLOCATES " &
      "TASKS OF A GLOBAL ACCESS TYPE, MUST TERMINATE " &
      "WITHOUT WAITING FOR THE ALLOCATED TASKS TO " &
      "TERMINATE IF AN EXCEPTION IS RAISED BUT NOT " &
      "HANDLED IN THE NON-MASTER UNIT");

   --------------------------------------------------

   declare -- (A)

      type A_T is access Tt;
      A1 : A_T;

   begin -- (A)

      declare
         A2 : A_T;
      begin
         A2 := new Tt;
         A2.all.E;
         A1 := A2;
         raise My_Exception;
         Failed ("MY_EXCEPTION WAS NOT RAISED IN (A)");
      end;

      abort A1.all;

   exception
      when My_Exception =>
         if A1.all'Terminated then
            Failed ("ALLOCATED TASK PREMATURELY TERMINATED - " & "(A)");
         else
            A1.all.E;
         end if;
      when others =>
         Failed ("UNEXPECTED EXCEPTION IN (A)");
         if A1 /= null then
            abort A1.all;
         end if;
   end; -- (A)

   --------------------------------------------------

   declare -- (B)

      I : Integer;

      function F return Integer is

         type Rt is record
            T : Tt;
         end record;
         type Art is access Rt;
         Ar1 : Art;

         procedure P is
            Ar2 : Art;
         begin
            Ar2 := new Rt;
            Ar2.T.E;
            Ar1 := Ar2;
            raise My_Exception;
            Failed ("MY_EXCEPTION WAS NOT RAISED IN (B)");
         end P;

      begin
         P;
         abort Ar1.T;
         return 0;
      exception
         when My_Exception =>
            if Ar1.T'Terminated then
               Failed ("ALLOCATED TASK PREMATURELY " & "TERMINATED - (B)");
            else
               Ar1.T.E;
            end if;
            return 0;
         when others =>
            Failed ("UNEXPECTED EXCEPTION IN (B)");
            if Ar1 /= null then
               abort Ar1.T;
            end if;
            return 0;
      end F;

   begin -- (B)

      I := F;

   end; -- (B)

   --------------------------------------------------

   declare -- (C)

      Loop_Count : Integer  := 0;
      Cut_Off    : constant := 60;                -- DELAY.

      task Tsk is
         entry Ent;
      end Tsk;

      task body Tsk is

         Loop_Count1 : Integer  := 0;
         Cut_Off1    : constant := 60;          -- DELAY.

         type Rat;
         type Arat is access Rat;
         type Arr is array (1 .. 1) of Tt;
         type Rat is record
            A : Arat;
            T : Arr;
         end record;
         Ara1 : Arat;

         task Tsk1 is
            entry Ent1 (Ara : out Arat);
         end Tsk1;

         task body Tsk1 is
            Ara2 : Arat;
         begin
            Ara2 := new Rat;         -- INITIATE TASK ARA2.T(1).
            Ara2.T (1).E;
            accept Ent1 (Ara : out Arat) do
               Ara := Ara2;
            end Ent1;
            raise My_Exception;       -- NOT PROPOGATED.
            Failed ("MY_EXCEPTION WAS NOT RAISED IN (C)");
         end Tsk1;

      begin
         Tsk1.Ent1 (Ara1);     -- ARA1.T BECOMES ALIAS FOR ARA2.T.

         while not Tsk1'Terminated and Loop_Count1 < Cut_Off1 loop
            delay 1.0;
            Loop_Count1 := Loop_Count1 + 1;
         end loop;

         if Loop_Count1 >= Cut_Off1 then
            Failed
              ("DEPENDENT TASK TSK1 NOT TERMINATED " &
               "WITHIN ONE MINUTE - (C)");
         end if;

         if Ara1.T (1)'Terminated then
            Failed ("ALLOCATED TASK PREMATURELY TERMINATED " & "- (C)");
         else
            Ara1.T (1).E;
         end if;
      end Tsk;

   begin -- (C)

      while not Tsk'Terminated and Loop_Count < Cut_Off loop
         delay 2.0;
         Loop_Count := Loop_Count + 1;
      end loop;

      if Loop_Count >= Cut_Off then
         Failed
           ("DEPENDENT TASK TSK NOT TERMINATED WITHIN " & "TWO MINUTES - (C)");
      end if;

   end; -- (C)

   --------------------------------------------------

   declare -- (D)

      Loop_Count : Integer  := 0;
      Cut_Off    : constant := 60;                -- DELAY.

      task Tsk is
         entry Ent;
      end Tsk;

      task body Tsk is

         Loop_Count1 : Integer  := 0;
         Cut_Off1    : constant := 60;          -- DELAY.

         package Pkg is
            type Lpt is limited private;
            procedure Call (X : Lpt);
            procedure Kill (X : Lpt);
            function Terminated (X : Lpt) return Boolean;
         private
            type Lpt is new Tt;
         end Pkg;

         use Pkg;

         type Alpt is access Lpt;
         Alp1 : Alpt;

         package body Pkg is
            procedure Call (X : Lpt) is
            begin
               X.E;
            end Call;

            procedure Kill (X : Lpt) is
            begin
               abort X;
            end Kill;

            function Terminated (X : Lpt) return Boolean is
            begin
               return X'Terminated;
            end Terminated;
         end Pkg;

         task Tsk1 is
            entry Ent1 (Alp : out Alpt);
            entry Die;
         end Tsk1;

         task body Tsk1 is
            Alp2 : Alpt;
         begin
            Alp2 := new Lpt;         -- INITIATE TASK ALP2.ALL.
            Call (Alp2.all);
            accept Ent1 (Alp : out Alpt) do
               Alp := Alp2;
            end Ent1;
            accept Die do
               raise My_Exception;       -- PROPOGATED.
               Failed ("MY_EXCEPTION WAS NOT RAISED IN (D)");
            end Die;
         end Tsk1;

      begin
         Tsk1.Ent1 (Alp1); -- ALP1.ALL BECOMES ALIAS FOR ALP2.ALL.
         Tsk1.Die;
         Failed ("MY_EXCEPTION WAS NOT PROPOGATED TO CALLING " & "TASK - (D)");
         Kill (Alp1.all);
         abort Tsk1;
      exception
         when My_Exception =>
            while not Tsk1'Terminated and Loop_Count1 < Cut_Off1 loop
               delay 1.0;
               Loop_Count1 := Loop_Count1 + 1;
            end loop;

            if Loop_Count1 >= Cut_Off1 then
               Failed
                 ("DEPENDENT TASK TSK1 NOT TERMINATED " &
                  "WITHIN ONE MINUTE - (D)");
            end if;

            if Terminated (Alp1.all) then
               Failed ("ALLOCATED TASK PREMATURELY " & "TERMINATED - (D)");
            else
               Call (Alp1.all);
            end if;
         when others =>
            Failed ("UNEXPECTED EXCEPTION IN (D)");
            if Alp1 /= null then
               Kill (Alp1.all);
            end if;
            abort Tsk1;
      end Tsk;

   begin -- (D)

      while not Tsk'Terminated and Loop_Count < Cut_Off loop
         delay 2.0;
         Loop_Count := Loop_Count + 1;
      end loop;

      if Loop_Count >= Cut_Off then
         Failed
           ("DEPENDENT TASK TSK NOT TERMINATED WITHIN " & "TWO MINUTES - (D)");
      end if;

   end; -- (D)

   --------------------------------------------------

   Result;
end C94002g;
