-- C41402A.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS NOT RAISED IF THE PREFIX OF
--     'ADDRESS, 'SIZE, 'FIRST_BIT, 'LAST_BIT, AND 'POSITION HAS THE
--     VALUE NULL.

-- HISTORY:
--     TBN  10/02/86  CREATED ORIGINAL TEST.
--     CJJ  07/01/87  REMOVED TEST FOR 'STORAGE_SIZE, WHICH IS NO LONGER
--                    PART OF THE OBJECTIVE.

with System; use System;
with Report; use Report;
procedure C41402a is

   type Array1 is array (1 .. 2) of Integer;
   type Acc_Ara is access Array1;

   Ptr_Ara : Acc_Ara;
   Var1    : Integer;

   type Rec1 is record
      A : Integer;
   end record;

   type Acc_Rec1 is access Rec1;

   type Rec2 is record
      P_Ar  : Acc_Ara;
      P_Rec : Acc_Rec1;
   end record;

   Obj_Rec : Rec2;

   procedure Proc (A : Address) is
   begin
      null;
   end Proc;

begin
   Test
     ("C41402A",
      "CHECK THAT CONSTRAINT_ERROR IS NOT RAISED IF " &
      "THE PREFIX OF 'ADDRESS, 'SIZE, " &
      "'FIRST_BIT, 'LAST_BIT, AND 'POSITION HAS THE " &
      "VALUE NULL");

   begin
      Proc (Ptr_Ara'Address);
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR 'ADDRESS");
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED 'ADDRESS");
   end;

   begin
      Var1 := Ptr_Ara'Size;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR 'SIZE");
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED 'SIZE");
   end;

   begin
      Var1 := Obj_Rec.P_Ar'First_Bit;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR 'FIRST_BIT");
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED 'FIRST_BIT");
   end;

   begin
      Var1 := Obj_Rec.P_Ar'Last_Bit;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR 'LAST_BIT");
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED 'LAST_BIT");
   end;

   begin
      Var1 := Obj_Rec.P_Rec'Position;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR 'POSITION");
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED 'POSITION");
   end;

   Result;
end C41402a;
