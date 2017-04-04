-- C41104A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED IF AN EXPRESSION GIVES AN INDEX VALUE
-- OUTSIDE THE RANGE SPECIFIED FOR THE INDEX FOR ARRAYS AND ACCESS TYPES.

-- TBN  9/12/86
-- EDS 8/03/98 AVOID OPTIMIZATION

with Report; use Report;
procedure C41104a is

   subtype Int is Integer range 1 .. 5;
   subtype Bool is Boolean range True .. True;
   subtype Char is Character range 'W' .. 'Z';
   type Array1 is array (Int range <>) of Integer;
   type Array2 is array (3 .. 1) of Integer;
   type Array3 is array (Bool range <>) of Integer;
   type Array4 is array (Char range <>) of Integer;

   type Rec (D : Int) is record
      A : Array1 (1 .. D);
   end record;

   type B_Rec (D : Bool) is record
      A : Array3 (True .. D);
   end record;

   type Null_Rec (D : Int) is record
      A : Array1 (D .. 1);
   end record;

   type Null_Crec (D : Char) is record
      A : Array4 (D .. 'W');
   end record;

begin
   Test
     ("C41104A",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED IF AN " &
      "EXPRESSION GIVES AN INDEX VALUE OUTSIDE THE " &
      "RANGE SPECIFIED FOR THE INDEX FOR ARRAYS AND " &
      "ACCESS TYPES");

   declare
      Ara1 : Array1 (1 .. 5) := (1, 2, 3, 4, 5);
   begin
      Ara1 (Ident_Int (0)) := 1;

      begin
         Failed
           ("CONSTRAINT_ERROR WAS NOT RAISED - " & Integer'Image (Ara1 (1)));
      exception
         when others =>
            Failed ("EXCEPTION ON ATTEMPT TO USE OBJECT");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - 1");
   end;
------------------------------------------------------------------------
   declare
      type Acc_Array is access Array3 (True .. True);
      Acc_Ara : Acc_Array := new Array3'(True => 2);
   begin
      Acc_Ara (Ident_Bool (False)) := 2;

      begin

         Failed
           ("CONSTRAINT_ERROR WAS NOT RAISED - " &
            Integer'Image (Acc_Ara (True)));
      exception
         when others =>
            Failed ("EXCEPTION ON ATTEMPT TO USE OBJECT");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - 2");
   end;
------------------------------------------------------------------------
   declare
      Ara2 : Array4 ('Z' .. 'Y');
   begin
      Ara2 (Ident_Char ('Y')) := 3;

      Failed ("CONSTRAINT_ERROR WAS NOT RAISED - 3");

      begin
         Comment ("ARA2 (Y) IS " & Integer'Image (Ara2 ('Y')));
      exception
         when others =>
            Failed ("EXCEPTION ON ATTEMPT TO USE OBJECT");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - 3");
   end;
------------------------------------------------------------------------
   declare
      type Acc_Array is access Array2;
      Acc_Ara : Acc_Array := new Array2;
   begin
      Acc_Ara (Ident_Int (4)) := 4;

      Failed ("CONSTRAINT_ERROR WAS NOT RAISED - 4");

      begin
         Comment ("ACC_ARA (4) IS " & Integer'Image (Acc_Ara (4)));
      exception
         when others =>
            Failed ("EXCEPTION ON ATTEMPT TO USE OBJECT");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - 4");
   end;
------------------------------------------------------------------------
   declare
      Rec1 : B_Rec (True) := (True, A => (True => 5));
   begin
      Rec1.A (Ident_Bool (False)) := 1;

      begin
         Failed
           ("CONSTRAINT_ERROR WAS NOT RAISED - " &
            Integer'Image (Rec1.A (True)));
      exception
         when others =>
            Failed ("EXCEPTION ON ATTEMPT TO USE OBJECT");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - 5");
   end;
------------------------------------------------------------------------
   declare
      type Acc_Rec is access Rec (3);
      Acc_Rec1 : Acc_Rec := new Rec'(3, (4, 5, 6));
   begin
      Acc_Rec1.A (Ident_Int (4)) := 4;

      begin
         Failed
           ("CONSTRAINT_ERROR WAS NOT RAISED - " &
            Integer'Image (Acc_Rec1.A (3)));
      exception
         when others =>
            Failed ("EXCEPTION ON ATTEMPT TO USE OBJECT");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - 6");
   end;
------------------------------------------------------------------------
   declare
      Rec1 : Null_Rec (2);
   begin
      Rec1.A (Ident_Int (2)) := 1;

      Failed ("CONSTRAINT_ERROR WAS NOT RAISED - 7");

      begin
         Comment ("REC1.A (2) IS " & Integer'Image (Rec1.A (2)));
      exception
         when others =>
            Failed ("EXCEPTION ON ATTEMPT TO USE OBJECT");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - 7");
   end;
------------------------------------------------------------------------
   declare
      type Acc_Rec is access Null_Crec ('Z');
      Acc_Rec1 : Acc_Rec := new Null_Crec ('Z');
   begin
      Acc_Rec1.A (Ident_Char ('A')) := 4;

      Failed ("CONSTRAINT_ERROR WAS NOT RAISED - 8");
      begin
         Comment ("ACC_REC1.A (A) IS " & Integer'Image (Acc_Rec1.A ('A')));
      exception
         when others =>
            Failed ("EXCEPTION ON ATTEMPT TO USE OBJECT");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - 8");
   end;
------------------------------------------------------------------------

   Result;
end C41104a;
