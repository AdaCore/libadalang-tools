-- C47007A.ADA

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
-- WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION DENOTES A CONSTRAINED
-- ARRAY TYPE, CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN THE BOUNDS
-- OF THE OPERAND ARE NOT THE SAME AS THE BOUNDS OF THE TYPE MARK.

-- RJW 7/23/86

with Report; use Report;
procedure C47007a is

   type Arr is array (Natural range <>) of Integer;

   type Tarr is array (Natural range <>, Natural range <>) of Integer;

   type Narr is new Arr;

   type Ntarr is new Tarr;

begin

   Test
     ("C47007A",
      "WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION " &
      "DENOTES A CONSTRAINED ARRAY TYPE, CHECK THAT " &
      "CONSTRAINT_ERROR IS RAISED WHEN THE BOUNDS " &
      "OF THE OPERAND ARE NOT THE SAME AS THE " &
      "BOUNDS OF THE TYPE MARK");

   declare

      subtype Sarr is Arr (Ident_Int (1) .. Ident_Int (1));
      A : Arr (Ident_Int (2) .. Ident_Int (2));
   begin
      A := Sarr'(A'Range => 0);
      Failed
        ("NO EXCEPTION RAISED WHEN BOUNDS NOT THE SAME AS " &
         "THOSE OF SUBTYPE SARR");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED WHEN BOUNDS NOT " &
            "THE SAME AS THOSE OF SUBTYPE SARR");
   end;

   declare

      subtype Nulla is Arr (Ident_Int (1) .. Ident_Int (0));
      A : Arr (Ident_Int (2) .. Ident_Int (1));

   begin
      A := Nulla'(A'First .. A'Last => 0);
      Failed
        ("NO EXCEPTION RAISED WHEN BOUNDS NOT THE SAME AS " &
         "THOSE OF SUBTYPE NULLA");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED WHEN BOUNDS NOT " &
            "THE SAME AS THOSE OF SUBTYPE NULLA");
   end;

   declare

      subtype Starr is
        Tarr (Ident_Int (1) .. Ident_Int (1), Ident_Int (1) .. Ident_Int (5));
      A : Tarr
        (Ident_Int (2) .. Ident_Int (6),
         Ident_Int (1) .. Ident_Int (1));
   begin
      A := Starr'(A'Range => (A'Range (2) => 0));
      Failed
        ("NO EXCEPTION RAISED WHEN BOUNDS NOT THE SAME AS " &
         "THOSE OF SUBTYPE STARR");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED WHEN BOUNDS NOT " &
            "THE SAME AS THOSE OF SUBTYPE STARR");
   end;

   declare

      subtype Nullt is
        Tarr (Ident_Int (1) .. Ident_Int (5), Ident_Int (1) .. Ident_Int (0));

      A : Tarr
        (Ident_Int (1) .. Ident_Int (5),
         Ident_Int (2) .. Ident_Int (1));
   begin
      A := Nullt'(A'First .. A'Last => (A'First (2) .. A'Last (2) => 0));
      Failed
        ("NO EXCEPTION RAISED WHEN BOUNDS NOT THE SAME AS " &
         "THOSE OF SUBTYPE NULLT");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED WHEN BOUNDS NOT " &
            "THE SAME AS THOSE OF SUBTYPE NULLT");
   end;

   declare

      subtype Snarr is Narr (Ident_Int (1) .. Ident_Int (1));
      A : Narr (Ident_Int (2) .. Ident_Int (2));

   begin
      A := Snarr'(A'Range => 0);
      Failed
        ("NO EXCEPTION RAISED WHEN BOUNDS NOT THE SAME AS " &
         "THOSE OF SUBTYPE SNARR");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED WHEN BOUNDS NOT " &
            "THE SAME AS THOSE OF SUBTYPE SNARR");
   end;

   declare

      subtype Nullna is Narr (Ident_Int (1) .. Ident_Int (0));
      A : Narr (Ident_Int (2) .. Ident_Int (1));

   begin
      A := Nullna'(A'Range => 0);
      Failed
        ("NO EXCEPTION RAISED WHEN BOUNDS NOT THE SAME AS " &
         "THOSE OF SUBTYPE NULLNA");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED WHEN BOUNDS NOT " &
            "THE SAME AS THOSE OF SUBTYPE NULLNA");
   end;

   declare

      subtype Sntarr is
        Ntarr (Ident_Int (1) .. Ident_Int (1), Ident_Int (1) .. Ident_Int (5));

      A : Ntarr
        (Ident_Int (2) .. Ident_Int (2),
         Ident_Int (1) .. Ident_Int (5));
   begin
      A := Sntarr'(A'Range => (A'Range (2) => 0));
      Failed
        ("NO EXCEPTION RAISED WHEN BOUNDS NOT THE SAME AS " &
         "THOSE OF SUBTYPE SNTARR");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED WHEN BOUNDS NOT " &
            "THE SAME AS THOSE OF SUBTYPE SNTARR");
   end;

   declare

      subtype Nullnt is
        Ntarr (Ident_Int (1) .. Ident_Int (5), Ident_Int (1) .. Ident_Int (0));

      A : Ntarr
        (Ident_Int (1) .. Ident_Int (5),
         Ident_Int (1) .. Ident_Int (1));
   begin
      A := Nullnt'(A'Range => (A'Range (2) => 0));
      Failed
        ("NO EXCEPTION RAISED WHEN BOUNDS NOT THE SAME AS " &
         "THOSE OF SUBTYPE NULLNT");
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED WHEN BOUNDS NOT " &
            "THE SAME AS THOSE OF SUBTYPE NULLNT");
   end;

   Result;
end C47007a;
