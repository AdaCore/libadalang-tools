-- C32112B.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED FOR THE DECLARATION OF A NULL
-- ARRAY OBJECT IF THE INITIAL VALUE IS NOT A NULL ARRAY.

-- RJW 7/20/86
-- GMT 7/01/87  ADDED CODE TO PREVENT DEAD VARIABLE OPTIMIZATION.
--              CHANGED THE RANGE VALUES OF A FEW DIMENSIONS.

with Report; use Report;

procedure C32112b is

   type Arr1 is array (Natural range <>) of Integer;
   subtype Narr1 is Arr1 (Ident_Int (2) .. Ident_Int (1));

   type Arr2 is array (Natural range <>, Natural range <>) of Integer;
   subtype Narr2 is
     Arr2 (Ident_Int (1) .. Ident_Int (2), Ident_Int (1) .. Ident_Int (0));

begin
   Test
     ("C32112B",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR " &
      "THE DECLARATION OF A NULL ARRAY OBJECT IF " &
      "THE INITIAL VALUE IS NOT A NULL ARRAY");

   begin
      declare
         A   : Arr1 (Ident_Int (1) .. Ident_Int (2));
         N1a : Narr1 := (A'Range => 0);
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'N1A'");
         A (1) := Ident_Int (N1a (1));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'N1A'");
   end;

   begin
      declare
         A   : Arr1 (Ident_Int (1) .. Ident_Int (2));
         N1b : constant Narr1 := (A'Range => 0);
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'N1B'");
         A (1) := Ident_Int (N1b (1));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'N1B'");
   end;

   begin
      declare
         A   : Arr1 (Ident_Int (1) .. Ident_Int (1));
         N1c : constant Narr1 := (A'Range => 0);
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'N1C'");
         A (1) := Ident_Int (N1c (1));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'N1C'");
   end;

   begin
      declare
         A   : Arr1 (Ident_Int (1) .. Ident_Int (1));
         N1d : Narr1 := (A'Range => 0);
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'N1D'");
         A (1) := Ident_Int (N1d (1));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'N1D'");
   end;

   begin
      declare
         A   : Arr1 (Ident_Int (0) .. Ident_Int (1));
         N1e : Arr1 (Ident_Int (1) .. Ident_Int (0)) := (A'Range => 0);
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'N1E'");
         A (1) := Ident_Int (N1e (1));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'N1E'");
   end;

   begin
      declare
         A   : Arr1 (Ident_Int (0) .. Ident_Int (1));
         N1f : constant Arr1 (Ident_Int (1) .. Ident_Int (0)) :=
           (A'Range => 0);
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'N1F'");
         A (1) := Ident_Int (N1f (1));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'N1F'");
   end;

   begin
      declare
         A : Arr2
           (Ident_Int (1) .. Ident_Int (2),
            Ident_Int (0) .. Ident_Int (1));
         N2a : constant Narr2 := (A'Range => (A'Range (2) => 0));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'N2'");
         A (1, 1) := Ident_Int (N2a (1, 1));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'N2A'");
   end;

   begin
      declare
         A : Arr2
           (Ident_Int (1) .. Ident_Int (2),
            Ident_Int (0) .. Ident_Int (1));
         N2b : Narr2 := (A'Range => (A'Range (2) => 0));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'N2B'");
         A (1, 1) := Ident_Int (N2b (1, 1));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'N2B'");
   end;

   begin
      declare
         A : Arr2
           (Ident_Int (1) .. Ident_Int (3),
            Ident_Int (1) .. Ident_Int (1));
         N2c : constant Narr2 := (A'Range => (A'Range (2) => 0));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'N2C'");
         A (1, 1) := Ident_Int (N2c (1, 1));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'N2C'");
   end;

   begin
      declare
         A : Arr2
           (Ident_Int (1) .. Ident_Int (3),
            Ident_Int (1) .. Ident_Int (1));
         N2d : Narr2 := (A'Range => (A'Range (2) => 0));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'N2D'");
         A (1, 1) := Ident_Int (N2d (1, 1));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'N2D'");
   end;

   begin
      declare
         A : Arr2
           (Ident_Int (1) .. Ident_Int (1),
            Ident_Int (1) .. Ident_Int (1));
         N2e : constant Arr2
           (Ident_Int (2) .. Ident_Int (1),
            Ident_Int (1) .. Ident_Int (1)) :=
           (A'Range => (A'Range (2) => 0));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'N2E'");
         A (1, 1) := Ident_Int (N2e (1, 1));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'N2E'");
   end;

   begin
      declare
         A : Arr2
           (Ident_Int (1) .. Ident_Int (1),
            Ident_Int (1) .. Ident_Int (1));
         N2f : Arr2
           (Ident_Int (2) .. Ident_Int (1),
            Ident_Int (1) .. Ident_Int (1)) :=
           (A'Range => (A'Range (2) => 0));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'N2F'");
         A (1, 1) := Ident_Int (N2f (1, 1));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'N2F'");
   end;

   Result;
end C32112b;
