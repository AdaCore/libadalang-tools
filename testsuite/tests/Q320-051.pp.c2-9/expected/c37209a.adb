-- C37209A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS NOT RAISED FOR A CONSTANT OBJECT
-- DECLARATION WHOSE SUBTYPE INDICATION SPECIFIES AN UNCONSTRAINED
-- TYPE WITH DEFAULT DISCRIMINANT VALUES AND WHOSE INITIALIZATION
-- EXPRESSION SPECIFIES A VALUE WHOSE DISCRIMINANTS ARE NOT EQUAL TO
-- THE DEFAULT VALUE.

-- R.WILLIAMS 8/25/86

with Report; use Report;
procedure C37209a is

begin
   Test
     ("C37209A",
      "CHECK THAT CONSTRAINT_ERROR IS NOT RAISED " &
      "FOR A CONSTANT OBJECT DECLARATION WHOSE " &
      "SUBTYPE INDICATION SPECIFIES AN " &
      "UNCONSTRAINED TYPE WITH DEFAULT " &
      "DISCRIMINANT VALUES AND WHOSE " &
      "INITIALIZATION EXPRESSION SPECIFIES A VALUE " &
      "WHOSE DISCRIMINANTS ARE NOT EQUAL TO THE " &
      "DEFAULT VALUE");
   declare

      type Rec1 (D : Integer := Ident_Int (5)) is record
         null;
      end record;

   begin
      declare
         R1 : constant Rec1 := (D => Ident_Int (10));
      begin
         Comment ("NO EXCEPTION RAISED AT DECLARATION OF R1");
      exception
         when others =>
            Failed ("EXCEPTION FOR R1 RAISED INSIDE BLOCK");
      end;

   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED AT DECLARATION OF " & "R1");
      when others =>
         Failed ("OTHER EXCEPTION RAISED AT DECLARATION OF " & "R1");
   end;

   begin
      declare
         package Priv is
            type Rec2 (D : Integer := Ident_Int (5)) is private;
            R2 : constant Rec2;

         private
            type Rec2 (D : Integer := Ident_Int (5)) is record
               null;
            end record;

            R2 : constant Rec2 := (D => Ident_Int (10));
         end Priv;

         use Priv;

      begin
         declare
            I : Integer := R2.D;
         begin
            Comment ("NO EXCEPTION RAISED AT DECLARATION " & "OF R2");
         end;
      end;

   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED AT DECLARATION OF " & "R2");
      when others =>
         Failed ("OTHER EXCEPTION RAISED AT DECLARATION " & "OF R2");
   end;

   begin
      declare
         package Lpriv is
            type Rec3 (D : Integer := Ident_Int (5)) is limited private;

            R3 : constant Rec3;

         private
            type Rec3 (D : Integer := Ident_Int (5)) is record
               null;
            end record;

            R3 : constant Rec3 := (D => Ident_Int (10));
         end Lpriv;

         use Lpriv;

      begin
         declare
            I : Integer;
         begin
            I := R3.D;
            Comment ("NO EXCEPTION RAISED AT DECLARATION " & "OF R3");
         end;
      end;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED AT DECLARATION OF " & "R3");
      when others =>
         Failed ("OTHER EXCEPTION RAISED AT DECLARATION " & "OF R3");
   end;

   Result;
end C37209a;
