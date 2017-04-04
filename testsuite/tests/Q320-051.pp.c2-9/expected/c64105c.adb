-- C64105C.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS NOT RAISED FOR ACCESS PARAMETERS
--   IN THE FOLLOWING CIRCUMSTANCES:
--       (1)
--       (2) AFTER THE CALL, WHEN AN IN OUT OR OUT FORMAL
--           ACCESS VALUE IS NULL, AND THE ACTUAL PARAMETER HAS
--           DIFFERENT CONSTRAINTS.
--       (3)
--   SUBTESTS ARE:
--       (C) CASE 2, IN OUT MODE, STATIC PRIVATE DISCRIMINANT.
--       (D) CASE 2, OUT MODE, DYNAMIC TWO DIMENSIONAL BOUNDS.
--       (E) SAME AS (C), WITH TYPE CONVERSION.
--       (F) SAME AS (D), WITH TYPE CONVERSION.

-- JRK 3/20/81
-- SPS 10/26/82
-- CPP 8/8/84

with Report;
procedure C64105c is

   use Report;

begin
   Test
     ("C64105C",
      "CHECK THAT CONSTRAINT_ERROR IS NOT RAISED " &
      "AFTER THE CALL, WHEN AN IN OUT OR OUT FORMAL " &
      "ACCESS VALUE IS NULL, AND THE ACTUAL PARAMETER HAS " &
      "DIFFERENT CONSTRAINTS");

   --------------------------------------------------

   declare -- (C)

      package Pkg is
         type E is (E1, E2);
         type T (D : E := E1) is private;
      private
         type T (D : E := E1) is record
            I : Integer;
            case D is
               when E1 =>
                  B : Boolean;
               when E2 =>
                  C : Character;
            end case;
         end record;
      end Pkg;
      use Pkg;

      type A is access T;
      subtype Sa is A (E2);
      V       : A (E1)  := null;
      Entered : Boolean := False;

      procedure P (X : in out Sa) is
      begin
         Entered := True;
         X       := null;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE - (C)");
      end P;

   begin -- (C)

      P (V);

   exception
      when Constraint_Error =>
         if not Entered then
            Failed ("EXCEPTION RAISED BEFORE CALL - (C)");
         else
            Failed ("EXCEPTION RAISED ON RETURN - (C)");
         end if;
      when others =>
         Failed ("EXCEPTION RAISED - (C)");
   end; -- (C)

   --------------------------------------------------

   declare -- (D)

      type T is array (Character range <>, Boolean range <>) of Integer;

      type A is access T;
      subtype Sa is A ('D' .. 'F', False .. False);
      V : A
        (Ident_Char ('A') .. Ident_Char ('B'),
         Ident_Bool (True) .. Ident_Bool (True)) :=
        null;
      Entered : Boolean := False;

      procedure P (X : out Sa) is
      begin
         Entered := True;
         X       := null;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE - (D)");
      end P;

   begin -- (D)

      P (V);

   exception
      when Constraint_Error =>
         if not Entered then
            Failed ("EXCEPTION RAISED BEFORE CALL - (D)");
         else
            Failed ("EXCEPTION RAISED ON RETURN - (D)");
         end if;
      when others =>
         Failed ("EXCEPTION RAISED - (D)");
   end; -- (D)

   --------------------------------------------------

   declare -- (E)

      package Pkg is
         type E is (E1, E2);
         type T (D : E := E1) is private;
      private
         type T (D : E := E1) is record
            I : Integer;
            case D is
               when E1 =>
                  B : Boolean;
               when E2 =>
                  C : Character;
            end case;
         end record;
      end Pkg;
      use Pkg;

      type A is access T;
      subtype Sa is A (E2);
      V       : A (E1)  := null;
      Entered : Boolean := False;

      procedure P (X : in out Sa) is
      begin
         Entered := True;
         X       := null;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE - (C)");
      end P;

   begin -- (E)

      P (Sa (V));

   exception
      when Constraint_Error =>
         if not Entered then
            Failed ("EXCEPTION RAISED BEFORE CALL - (E)");
         else
            Failed ("EXCEPTION RAISED ON RETURN - (E)");
         end if;
      when others =>
         Failed ("EXCEPTION RAISED - (E)");
   end; -- (E)

   --------------------------------------------------

   declare -- (F)

      type T is array (Character range <>, Boolean range <>) of Integer;

      type A is access T;
      subtype Sa is A ('D' .. 'F', False .. False);
      V : A
        (Ident_Char ('A') .. Ident_Char ('B'),
         Ident_Bool (True) .. Ident_Bool (True)) :=
        null;
      Entered : Boolean := False;

      procedure P (X : out Sa) is
      begin
         Entered := True;
         X       := null;
      exception
         when others =>
            Failed ("EXCEPTION RAISED IN PROCEDURE - (D)");
      end P;

   begin -- (D)

      P (Sa (V));

   exception
      when Constraint_Error =>
         if not Entered then
            Failed ("EXCEPTION RAISED BEFORE CALL - (F)");
         else
            Failed ("EXCEPTION RAISED ON RETURN - (F)");
         end if;
      when others =>
         Failed ("EXCEPTION RAISED - (F)");
   end; -- (F)

   --------------------------------------------------

   Result;
end C64105c;
