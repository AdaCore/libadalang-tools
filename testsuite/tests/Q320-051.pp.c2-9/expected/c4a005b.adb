-- C4A005B.ADA

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
-- CHECK THAT A NONSTATIC UNIVERSAL INTEGER EXPRESSION RAISES CONSTRAINT_ERROR
-- IF DIVISION BY ZERO IS ATTEMPTED OR IF THE SECOND OPERAND OF REM OR MOD IS
-- ZERO.

-- *** NOTE: This test has been modified since ACVC version 1.11 to -- 9X ***
-- remove incompatibilities associated with the transition -- 9X *** to Ada 9X.
-- -- 9X *** -- 9X

-- JBG 5/2/85
-- EG 10/24/85 FIX NUMERIC_ERROR/CONSTRAINT_ERROR ACCORDING TO
--               AI-00387; PREVENT DEAD VARIABLE OPTIMIZATION
-- MRM 03/30/93 REMOVE NUMERIC_ERROR FOR 9X COMPATIBILITY

with Report; use Report;
procedure C4a005b is
begin
   Test
     ("C4A005B",
      "CHECK CONSTRAINT_ERROR FOR " & "NONSTATIC UNIVERSAL " &
      "INTEGER EXPRESSIONS - DIVISION BY ZERO");
   begin
      declare
         X : Boolean := 1 = 1 / Integer'Pos (Ident_Int (0));
      begin
         Failed ("CONSTRAINT_ERROR NOT RAISED - DIV");
         if X /= Ident_Bool (X) then
            Failed ("WRONG RESULT - DIV");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION IN WRONG PLACE - DIV");
      end;
   exception
      when Constraint_Error =>
         Comment ("CONSTRAINT_ERROR RAISED FOR / BY 0");
      when others =>
         Failed ("WRONG EXCEPTION RAISED - DIV");
   end;

   begin
      declare
         X : Boolean := 1 = 1 rem Integer'Pos (Ident_Int (0));
      begin
         Failed ("CONSTRAINT_ERROR NOT RAISED - REM");
         if X /= Ident_Bool (X) then
            Failed ("WRONG RESULT - REM");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION IN WRONG PLACE - REM");
      end;
   exception
      when Constraint_Error =>
         Comment ("CONSTRAINT_ERROR RAISED FOR REM BY 0");
      when others =>
         Failed ("WRONG EXCEPTION RAISED - REM");
   end;

   begin
      declare
         X : Boolean := 1 = Integer'Pos (Ident_Int (1)) mod 0;
      begin
         Failed ("CONSTRAINT_ERROR NOT RAISED - MOD");
         if X /= Ident_Bool (X) then
            Failed ("WRONG RESULT - MOD");
         end if;
      exception
         when others =>
            Failed ("EXCEPTION IN WRONG PLACE - MOD");
      end;
   exception
      when Constraint_Error =>
         Comment ("CONSTRAINT_ERROR RAISED FOR MOD BY 0");
      when others =>
         Failed ("WRONG EXCEPTION RAISED - MOD");
   end;

   Result;

end C4a005b;
