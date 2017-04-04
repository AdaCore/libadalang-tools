-- C87B26B.ADA

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
-- CHECK THAT 'ADDRESS, 'CONSTRAINED, 'SIZE, AND 'STORAGE_SIZE MAY BE USED WITH
-- THE DESIGNATED OBJECTS OF ACCESS VALUES RETURNED FROM OVERLOADED FUNCTIONS,
-- AND THAT EXPLICIT DEREFERENCING IS USED BY OVERLOADING RESOLUTION TO RESOLVE
-- THE PREFIXES OF THE ATTRIBUTES.

-- DSJ 22 JUN 83
-- JBG 11/22/83
-- JBG 4/23/84
-- JBG 5/25/85
-- RLB 3/16/07 CORRECTED ILLEGAL (BY AMENDMENT 1) RETURN.

with Report;
with System;
use Report;
use System;

procedure C87b26b is

   type Rec (D : Integer) is record
      C1, C2 : Integer;
   end record;
   type P_Rec is access Rec;

   P_Rec_Object : P_Rec := new Rec'(1, 1, 1);

   type Big_Int is range 0 .. System.Max_Int;
   task type Task_Type is
   -- NOTHING AT ALL
   end Task_Type;

   type P_Task is access Task_Type;

   P_Task_Object : P_Task;

   task body Task_Type is
   begin
      null;
   end Task_Type;

   ------------------------------------------------------------

   function F return Rec is
   begin
      return (0, 0, 0);
   end F;

   function F return P_Rec is
   begin
      return P_Rec_Object;
   end F;

   ------------------------------------------------------------

   function G return Task_Type is
   begin
      return New_Task : Task_Type;
   end G;

   function G return P_Task is
   begin
      return P_Task_Object;
   end G;

------------------------------------------------------------

begin

   Test
     ("C87B26B",
      "CHECK THAT EXPLICIT DEREFERENCING IN AN " &
      "ATTRIBUTE PREFIX IS USED IN OVERLOADING RESOLUTION " &
      "WITH 'ADDRESS, 'CONSTRAINED, 'SIZE, AND 'STORAGE_SIZE");

   declare

      A : Address;   -- FOR 'ADDRESS OF RECORD
      B : Boolean;   -- FOR 'CONSTRAINED OF RECORD
      C : Integer;   -- FOR 'SIZE OF RECORD
      D : Address;   -- FOR 'ADDRESS OF TASK
      E : Big_Int;   -- FOR 'STORAGE_SIZE OF TASK

   begin

      P_Task_Object := new Task_Type;
      A             := F.all'Address;
      B             := F.all'Constrained;
      C             := F.all'Size;
      D             := G.all'Address;
      E             := G.all'Storage_Size;

      if A /= P_Rec_Object.all'Address then
         Failed ("INCORRECT RESOLUTION FOR 'ADDRESS - REC");
      end if;

      if B /= P_Rec_Object.all'Constrained then
         Failed ("INCORRECT RESOLUTION FOR 'CONSTRAINED");
      end if;

      if C /= P_Rec_Object.all'Size then
         Failed ("INCORRECT RESOLUTION FOR 'SIZE");
      end if;

      if D /= P_Task_Object.all'Address then
         Failed ("INCORRECT RESOLUTION FOR 'ADDRESS - TASK");
      end if;

      if E /= P_Task_Object.all'Storage_Size then
         Failed ("INCORRECT RESOLUTION FOR 'STORAGE_SIZE");
      end if;

      if A = P_Rec_Object'Address then
         Failed ("INCORRECT DEREFERENCING FOR 'ADDRESS - REC");
      end if;

      if C = P_Rec_Object'Size and C /= P_Rec_Object.all'Size then
         Failed ("INCORRECT DEREFERENCING FOR 'SIZE");
      end if;

      if D = P_Task_Object'Address then
         Failed ("INCORRECT DEREFERENCING FOR 'ADDRESS - TASK");
      end if;

   end;

   Result;

end C87b26b;
