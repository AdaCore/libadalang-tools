-- C41401A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED IF THE PREFIX OF THE FOLLOWING
-- ATTRIBUTES HAS THE VALUE NULL:
--     A) 'CALLABLE AND 'TERMINATED FOR A TASK TYPE.
--     B) 'FIRST, 'FIRST(N), 'LAST, 'LAST(N), 'LENGTH, 'LENGTH(N),
--        'RANGE, AND 'RANGE(N) FOR AN ARRAY TYPE.

-- TBN  10/2/86
-- EDS 07/14/98 AVOID OPTIMIZATION

with Report; use Report;
procedure C41401a is

   subtype Int is Integer range 1 .. 10;

   task type Tt is
      entry E;
   end Tt;

   type Acc_Tt is access Tt;

   type Null_Arr1 is array (2 .. 1) of Integer;
   type Array1 is array (Int range <>) of Integer;
   type Null_Arr2 is array (3 .. 1, 2 .. 1) of Integer;
   type Array2 is array (Int range <>, Int range <>) of Integer;
   type Acc_Null1 is access Null_Arr1;
   type Acc_Arr1 is access Array1;
   type Acc_Null2 is access Null_Arr2;
   type Acc_Arr2 is access Array2;

   Ptr_Tt   : Acc_Tt;
   Ptr_Ara1 : Acc_Null1;
   Ptr_Ara2 : Acc_Arr1 (1 .. 4);
   Ptr_Ara3 : Acc_Null2;
   Ptr_Ara4 : Acc_Arr2 (1 .. 2, 2 .. 4);
   Bool_Var : Boolean := False;
   Int_Var  : Integer := 1;

   task body Tt is
   begin
      accept E;
   end Tt;

begin
   Test
     ("C41401A",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED IF THE " &
      "PREFIX HAS A VALUE OF NULL FOR THE FOLLOWING " &
      "ATTRIBUTES: 'CALLABLE, 'TERMINATED, 'FIRST, " &
      "'LAST, 'LENGTH, AND 'RANGE");

   begin
      if Equal (3, 2) then
         Ptr_Tt := new Tt;
      end if;
      Bool_Var := Ident_Bool (Ptr_Tt'Callable);
      Failed ("CONSTRAINT_ERROR NOT RAISED - 1 " & Boolean'Image (Bool_Var));
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 2");
   end;

   begin
      if Equal (1, 3) then
         Ptr_Tt := new Tt;
      end if;
      Bool_Var := Ident_Bool (Ptr_Tt'Terminated);
      Failed ("CONSTRAINT_ERROR NOT RAISED - 3 " & Boolean'Image (Bool_Var));
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 4");
   end;

   begin
      Int_Var := Ident_Int (Ptr_Ara1'First);
      Failed ("CONSTRAINT_ERROR NOT RAISED - 5 " & Integer'Image (Int_Var));
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 6");
   end;

   begin
      Int_Var := Ident_Int (Ptr_Ara2'Last);
      Failed ("CONSTRAINT_ERROR NOT RAISED - 7 " & Integer'Image (Int_Var));
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 8");
   end;

   begin
      Int_Var := Ident_Int (Ptr_Ara1'Length);
      Failed ("CONSTRAINT_ERROR NOT RAISED - 9 " & Integer'Image (Int_Var));
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 10");
   end;

   begin
      declare
         A : Array1 (Ptr_Ara2'Range);
      begin
         A (1) := Ident_Int (1);
         Failed ("CONSTRAINT_ERROR NOT RAISED - 11 " & Integer'Image (A (1)));
      exception
         when others =>
            Failed ("CONSTRAINT_ERROR NOT RAISED - 11 ");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 12");
   end;

   begin
      Int_Var := Ident_Int (Ptr_Ara3'First (2));
      Failed ("CONSTRAINT_ERROR NOT RAISED - 13 " & Integer'Image (Int_Var));
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 14");
   end;

   begin
      Int_Var := Ident_Int (Ptr_Ara4'Last (2));
      Failed ("CONSTRAINT_ERROR NOT RAISED - 15 " & Integer'Image (Int_Var));
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 16");
   end;

   begin
      Int_Var := Ident_Int (Ptr_Ara3'Length (2));
      Failed ("CONSTRAINT_ERROR NOT RAISED - 17 " & Integer'Image (Int_Var));
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 18");
   end;

   begin
      declare
         A : Array1 (Ptr_Ara4'Range (2));
      begin
         A (1) := Ident_Int (1);
         Failed ("CONSTRAINT_ERROR NOT RAISED - 19 " & Integer'Image (A (1)));
      exception
         when others =>
            Failed ("CONSTRAINT_ERROR NOT RAISED - 19 ");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 20");
   end;

   begin
      Int_Var := Ident_Int (Ptr_Ara4'Last (1));
      Failed ("CONSTRAINT_ERROR NOT RAISED - 21 " & Integer'Image (Int_Var));
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 22");
   end;

   begin
      Int_Var := Ident_Int (Ptr_Ara3'Length (1));
      Failed ("CONSTRAINT_ERROR NOT RAISED - 23 " & Integer'Image (Int_Var));
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 24");
   end;

   Result;
end C41401a;
