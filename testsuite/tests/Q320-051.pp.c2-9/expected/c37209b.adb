-- C37209B.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN THE SUBTYPE
--     INDICATION IN A CONSTANT OBJECT DECLARATION SPECIFIES A
--     CONSTRAINED SUBTYPE WITH DISCRIMINANTS AND THE INITIALIZATION
--     VALUE DOES NOT BELONG TO THE SUBTYPE (I. E., THE DISCRIMINANT
--     VALUE DOES NOT MATCH THOSE SPECIFIED BY THE CONSTRAINT).

-- HISTORY:
--     RJW 08/25/86  CREATED ORIGINAL TEST
--     VCL 08/19/87  CHANGED THE RETURN TYPE OF FUNTION 'INIT' IN
--                   PACKAGE 'PRIV2' SO THAT 'INIT' IS UNCONSTRAINED,
--                   THUS NOT RAISING A CONSTRAINT ERROR ON RETURN FROM
--                   'INIT'.

with Report; use Report;
procedure C37209b is

begin
   Test
     ("C37209B",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN " &
      "THE SUBTYPE INDICATION IN A CONSTANT " &
      "OBJECT DECLARATION SPECIFIES A CONSTRAINED " &
      "SUBTYPE WITH DISCRIMINANTS AND THE " &
      "INITIALIZATION VALUE DOES NOT BELONG TO " &
      "THE SUBTYPE (I. E., THE DISCRIMINANT VALUE " &
      "DOES NOT MATCH THOSE SPECIFIED BY THE " &
      "CONSTRAINT)");
   declare

      type Rec (D : Integer) is record
         null;
      end record;

      subtype Rec1 is Rec (Ident_Int (5));
   begin
      declare
         R1 : constant Rec1 := (D => Ident_Int (10));
         I  : Integer       := Ident_Int (R1.D);
      begin
         Failed ("NO EXCEPTION RAISED FOR DECLARATION OF " & "R1");
      exception
         when others =>
            Failed ("EXCEPTION FOR R1 RAISED INSIDE BLOCK");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED AT DECLARATION OF " & "R1");
   end;

   begin
      declare
         package Priv1 is
            type Rec (D : Integer) is private;
            subtype Rec2 is Rec (Ident_Int (5));
            R2 : constant Rec2;

         private
            type Rec (D : Integer) is record
               null;
            end record;

            R2 : constant Rec2 := (D => Ident_Int (10));
         end Priv1;

         use Priv1;

      begin
         declare
            I : Integer := Ident_Int (R2.D);
         begin
            Failed ("NO EXCEPTION RAISED AT DECLARATION " & "OF R2");
         end;
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED AT DECLARATION " & "OF R2");
   end;

   begin
      declare
         package Priv2 is
            type Rec (D : Integer) is private;
            subtype Rec3 is Rec (Ident_Int (5));

            function Init (D : Integer) return Rec;
         private
            type Rec (D : Integer) is record
               null;
            end record;

         end Priv2;

         package body Priv2 is
            function Init (D : Integer) return Rec is
            begin
               return (D => Ident_Int (D));
            end Init;
         end Priv2;

         use Priv2;

      begin
         declare
            R3 : constant Rec3 := Init (10);
            I  : Integer       := Ident_Int (R3.D);
         begin
            Failed ("NO EXCEPTION RAISED AT DECLARATION " & "OF R3");
         end;
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED AT DECLARATION " & "OF R3");
   end;

   begin
      declare
         package Lpriv is
            type Rec (D : Integer) is limited private;
            subtype Rec4 is Rec (Ident_Int (5));

            R4 : constant Rec4;

         private
            type Rec (D : Integer) is record
               null;
            end record;

            R4 : constant Rec4 := (D => Ident_Int (10));
         end Lpriv;

         use Lpriv;

      begin
         declare
            I : Integer := Ident_Int (R4.D);
         begin
            Failed ("NO EXCEPTION RAISED AT DECLARATION " & "OF R4");
         end;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED AT DECLARATION " & "OF R4");
   end;

   Result;
end C37209b;
