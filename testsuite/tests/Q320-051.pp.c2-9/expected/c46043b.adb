-- C46043B.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED FOR CONVERSION TO AN UNCONSTRAINED
-- ARRAY TYPE IF, FOR A NON-NULL DIMENSION OF THE OPERAND TYPE, ONE BOUND
-- DOES NOT BELONG TO THE CORRESPONDING INDEX SUBTYPE OF THE TARGET TYPE.

-- R.WILLIAMS 9/8/86

with Report; use Report;
procedure C46043b is

   subtype Subint is Integer range Ident_Int (0) .. Ident_Int (9);

begin
   Test
     ("C46043B",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR " &
      "CONVERSION TO AN UNCONSTRAINED ARRAY TYPE " &
      "IF, FOR A NON-NULL DIMENSION OF THE OPERAND " &
      "TYPE, ONE BOUND DOES NOT BELONG TO THE " &
      "CORRESPONDING INDEX SUBTYPE OF THE TARGET " & "TYPE");

   declare
      type Arr1 is array (Integer range <>) of Integer;
      A1 : Arr1 (Ident_Int (1) .. Ident_Int (10));

      type Arr2 is array (Subint range <>) of Integer;

      procedure Check (A : Arr2) is
      begin
         Failed ("NO EXCEPTION RAISED WITH ONE DIMENSIONAL " & "ARRAYS");
      end Check;

   begin
      A1 := (A1'Range => 0);
      Check (Arr2 (A1));
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED WITH ONE " & "DIMENSIONAL ARRAYS");
   end;

   declare
      type Arr1 is array (Integer range <>, Integer range <>) of Integer;
      A1 : Arr1 (Ident_Int (1) .. Ident_Int (10),
         Ident_Int (1) .. Ident_Int (1));

      type Arr2 is array (Subint range <>, Integer range <>) of Integer;

      procedure Check (A : Arr2) is
      begin
         Failed ("NO EXCEPTION RAISED WITH TWO DIMENSIONAL " & "ARRAYS");
      end Check;

   begin
      A1 := (A1'Range (1) => (A1'Range (2) => 0));
      Check (Arr2 (A1));
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED WITH TWO " & "DIMENSIONAL ARRAYS");
   end;

   declare
      type Arr1 is array (Integer range <>, Integer range <>) of Integer;
      A1 : Arr1 (Ident_Int (1) .. Ident_Int (10),
         Ident_Int (1) .. Ident_Int (0));

      type Arr2 is array (Subint range <>, Integer range <>) of Integer;

      procedure Check (A : Arr2) is
      begin
         Failed ("NO EXCEPTION RAISED WITH NULL ARRAYS - 1");
      end Check;

   begin
      A1 := (A1'Range (1) => (A1'Range (2) => 0));
      Check (Arr2 (A1));
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED " & "WITH NULL ARRAYS - 1");
   end;

   declare
      type Arr1 is array (Integer range <>, Integer range <>) of Integer;
      A1 : Arr1 (Ident_Int (1) .. Ident_Int (10),
         Ident_Int (1) .. Ident_Int (0));

      subtype Noint is Integer range Ident_Int (1) .. Ident_Int (0);

      type Arr2 is array (Subint range <>, Noint range <>) of Integer;

      procedure Check (A : Arr2) is
      begin
         Failed ("NO EXCEPTION RAISED WITH NULL ARRAYS - 2");
      end Check;

   begin
      A1 := (A1'Range (1) => (A1'Range (2) => 0));
      Check (Arr2 (A1));
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED " & "WITH NULL ARRAYS - 2");
   end;

   Result;
end C46043b;
