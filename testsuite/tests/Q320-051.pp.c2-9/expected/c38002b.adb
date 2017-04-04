-- C38002B.ADA

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
--     CHECK THAT AN UNCONSTRAINED ARRAY TYPE OR A RECORD WITHOUT
--     DEFAULT DISCRIMINANTS CAN BE USED IN AN ACCESS_TYPE_DEFINITION
--     WITHOUT AN INDEX OR DISCRIMINANT CONSTRAINT.
--
--     CHECK THAT (NON-STATIC) INDEX OR DISCRIMINANT CONSTRAINTS CAN
--     SUBSEQUENTLY BE IMPOSED WHEN THE TYPE IS USED IN AN OBJECT
--     DECLARATION, ARRAY COMPONENT DECLARATION, RECORD COMPONENT
--     DECLARATION, ACCESS TYPE DECLARATION, PARAMETER DECLARATION,
--     ALLOCATOR, DERIVED TYPE DEFINITION, PRIVATE TYPE, OR AS THE
--     RETURN TYPE IN A FUNCTION DECLARATION.
--
--     CHECK FOR GENERIC FORMAL ACCESS TYPES.

-- HISTORY:
--     AH  09/02/86 CREATED ORIGINAL TEST.
--     DHH 08/22/88 REVISED HEADER, ADDED 'PRIVATE TYPE' TO COMMENTS
--                  AND CORRECTED INDENTATION.

with Report; use Report;
procedure C38002b is

   C3 : constant Integer := Ident_Int (3);

   type Uncon_Arr is array (Integer range <>) of Integer;
   type Rec (Disc : Integer) is record
      null;
   end record;

   type P_Arr_Name is access Uncon_Arr;
   type P_Rec_Name is access Rec;

   generic
      type Acc_Rec is access Rec;
      type Acc_Arr is access Uncon_Arr;
   package P is
      Obj : Acc_Rec (C3);

      type Arr2 is array (1 .. 10) of Acc_Rec (C3);

      type Rec1 is record
         Comp1 : Acc_Rec (C3);
      end record;

      type Rec2 is record
         Comp2 : Acc_Arr (1 .. C3);
      end record;

      subtype Acc_Rec_3 is Acc_Rec (C3);
      R : Acc_Rec;

      function F (Parm : Acc_Rec_3) return Acc_Rec_3;

      type Acc1 is private;
      type Acc2 is private;
      type Der1 is private;
      type Der2 is private;

   private

      type Acc1 is access Acc_Rec (C3);
      type Acc2 is access Acc_Arr (1 .. C3);
      type Der1 is new Acc_Rec (C3);
      type Der2 is new Acc_Arr (1 .. C3);
   end P;

   package body P is
      function F (Parm : Acc_Rec_3) return Acc_Rec_3 is
      begin
         return Parm;
      end F;
   end P;

   package Np is new P (Acc_Rec => P_Rec_Name, Acc_Arr => P_Arr_Name);

   use Np;
begin
   Test
     ("C38002B",
      "NON-STATIC CONSTRAINTS CAN BE IMPOSED " &
      "ON ACCESS TYPES ACCESSING PREVIOUSLY UNCONSTRAINED " &
      "ARRAY OR RECORD TYPES");

   R := new Rec (Disc => 3);
   R := F (R);
   R := new Rec (Disc => 4);
   R := F (R);
   Failed
     ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE ACCEPTED " &
      "BY GENERIC FUNCTION");
exception
   when Constraint_Error =>
      if R = null or else R.Disc /= 4 then
         Failed
           (" ERROR IN EVALUATION/ASSIGNMENT OF " & "GENERIC ACCESS VALUE");
      end if;

      Result;
end C38002b;
