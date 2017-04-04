-- C46044B.ADA

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
-- CHECK THAT CONSTRAINT ERROR IS RAISED FOR CONVERSION TO A CONSTRAINED
-- ARRAY TYPE IF THE TARGET TYPE IS NON-NULL AND CORRESPONDING DIMENSIONS
-- OF THE TARGET AND OPERAND DO NOT HAVE THE SAME LENGTH. ALSO, CHECK THAT
-- CONSTRAINT_ERROR IS RAISED IF THE TARGET TYPE IS NULL AND THE OPERAND
-- TYPE IS NON-NULL.

-- R.WILLIAMS 9/8/86

with Report; use Report;
procedure C46044b is

   type Arr1 is array (Integer range <>) of Integer;

   subtype Carr1a is Arr1 (Ident_Int (1) .. Ident_Int (6));
   C1a : Carr1a := (Carr1a'Range => 0);

   subtype Carr1b is Arr1 (Ident_Int (2) .. Ident_Int (5));
   C1b : Carr1b := (Carr1b'Range => 0);

   subtype Carr1n is Arr1 (Ident_Int (1) .. Ident_Int (0));
   C1n : Carr1n := (Carr1n'Range => 0);

   type Arr2 is array (Integer range <>, Integer range <>) of Integer;

   subtype Carr2a is
     Arr2 (Ident_Int (1) .. Ident_Int (2), Ident_Int (1) .. Ident_Int (2));
   C2a : Carr2a := (Carr2a'Range (1) => (Carr2a'Range (2) => 0));

   subtype Carr2b is
     Arr2 (Ident_Int (0) .. Ident_Int (2), Ident_Int (0) .. Ident_Int (2));
   C2b : Carr2b := (Carr2b'Range (1) => (Carr2b'Range (2) => 0));

   subtype Carr2n is
     Arr2 (Ident_Int (2) .. Ident_Int (1), Ident_Int (1) .. Ident_Int (2));
   C2n : Carr2n := (Carr2n'Range (1) => (Carr2n'Range (2) => 0));

   procedure Check1 (A : Arr1; Str : String) is
   begin
      Failed ("NO EXCEPTION RAISED - " & Str);
   end Check1;

   procedure Check2 (A : Arr2; Str : String) is
   begin
      Failed ("NO EXCEPTION RAISED - " & Str);
   end Check2;

begin
   Test
     ("C46044B",
      "CHECK THAT CONSTRAINT ERROR IS RAISED FOR " &
      "CONVERSION TO A CONSTRAINED ARRAY TYPE " &
      "IF THE TARGET TYPE IS NON-NULL AND " &
      "CORRESPONDING DIMENSIONS OF THE TARGET AND " &
      "OPERAND DO NOT HAVE THE SAME LENGTH. " &
      "ALSO, CHECK THAT CONSTRAINT_ERROR IS " &
      "RAISED IF THE TARGET TYPE IS NULL AND " &
      "THE OPERAND TYPE IS NON-NULL");

   begin -- (A).
      C1a := C1b;
      Check1 (C1a, "(A)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (A)");
   end;

   begin -- (B).
      Check1 (Carr1a (C1b), "(B)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (B)");
   end;

   begin -- (C).
      C1b := C1a;
      Check1 (C1b, "(C)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (C)");
   end;

   begin -- (D).
      Check1 (Carr1b (C1a), "(D)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (D)");
   end;

   begin -- (E).
      C1a := C1n;
      Check1 (C1a, "(E)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (E)");
   end;

   begin -- (F).
      Check1 (Carr1a (C1n), "(F)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (F)");
   end;

   begin -- (G).
      C2a := C2b;
      Check2 (C2a, "(G)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (G)");
   end;

   begin -- (H).
      Check2 (Carr2a (C2b), "(H)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (H)");
   end;

   begin -- (I).
      C2b := C2a;
      Check2 (C2b, "(I)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (I)");
   end;

   begin -- (J).
      Check2 (Carr2a (C2b), "(J)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (J)");
   end;

   begin -- (K).
      C2a := C2n;
      Check2 (C2a, "(K)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (K)");
   end;

   begin -- (L).
      Check2 (Carr2a (C2n), "(L)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (L)");
   end;

   begin -- (M).
      C1n := C1a;
      Check1 (C1n, "(M)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (M)");
   end;

   begin -- (N).
      Check1 (Carr1n (C1a), "(N)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (N)");
   end;

   begin -- (O).
      C2n := C2a;
      Check2 (C2n, "(O)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (O)");
   end;

   begin -- (P).
      Check2 (Carr2n (C2a), "(P)");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - (P)");
   end;

   Result;
end C46044b;
