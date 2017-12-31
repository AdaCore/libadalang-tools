-- C37108B.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED IN AN OBJECT DECLARATION IF A DEFAULT
-- INITIAL VALUE HAS BEEN SPECIFIED WHICH VIOLATES THE CONSTRAINTS OF A RECORD
-- OR AN ARRAY TYPE WHOSE CONSTRAINT DEPENDS ON A DISCRIMINANT, AND NO EXPLICIT
-- INITIALIZATION IS PROVIDED FOR THE OBJECT.

-- R.WILLIAMS 8/25/86
-- EDS 7/16/98 AVOID OPTIMIZATION

with Report; use Report;
procedure C37108b is

   type Arr is array (Positive range <>) of Integer;

   type R (P : Positive) is record
      null;
   end record;

begin
   Test
     ("C37108B",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED IN " &
      "AN OBJECT DECLARATION IF A DEFAULT INITIAL " &
      "VALUE HAS BEEN SPECIFIED WHICH VIOLATES THE " &
      "CONSTRAINTS OF A RECORD OR AN ARRAY TYPE " &
      "WHOSE CONSTRAINT DEPENDS ON A DISCRIMINANT, " &
      "AND NO EXPLICIT INITIALIZATION IS PROVIDED " & "FOR THE OBJECT");

   begin
      declare
         type Rec1 (D : Natural := Ident_Int (0)) is record
            A : Arr (D .. 5);
         end record;

      begin
         declare
            R1 : Rec1;

         begin
            R1.A (1) := Ident_Int (2);
            Failed
              ("NO EXCEPTION RAISED AT DECLARATION OF " & "R1" &
               Integer'Image (R1.A (5)));  --USE R2
         exception
            when others =>
               Failed ("EXCEPTION FOR R1 RAISED INSIDE " & "BLOCK");
         end;

      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED AT DECLARATION " & "OF R1");
      end;

   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR TYPE " & "DECLARATION OF REC1");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR TYPE " & "DECLARATION OF REC1");
   end;

   begin
      declare
         type Rec2 (D : Integer := Ident_Int (-1)) is record
            A : R (P => D);
         end record;

      begin
         declare
            R2 : Rec2;

         begin
            R2.A := R'(P => Ident_Int (1));
            Failed
              ("NO EXCEPTION RAISED AT DECLARATION OF " & "R2" &
               Integer'Image (R2.A.P));  --USE R2
         exception
            when others =>
               Failed ("EXCEPTION FOR R2 RAISED INSIDE " & "BLOCK");
         end;

      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED AT DECLARATION " & "OF R2");
      end;

   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR TYPE " & "DECLARATION OF REC2");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR TYPE " & "DECLARATION OF REC2");
   end;

   begin
      declare
         package Priv is
            type Rec3 (D : Integer := Ident_Int (-1)) is private;
            procedure Proc (R : Rec3);

         private
            type Rec3 (D : Integer := Ident_Int (-1)) is record
               A : R (P => D);
            end record;
         end Priv;

         package body Priv is
            procedure Proc (R : Rec3) is
               I : Integer;
            begin
               I := Ident_Int (R.A.P);
               if Equal (2, Ident_Int (1)) then
                  Failed ("IMPOSSIBLE " & Integer'Image (I));  --USE I
               end if;
            end Proc;
         end Priv;

         use Priv;

      begin
         declare
            R3 : Rec3;

         begin
            Proc (R3);
            Failed ("NO EXCEPTION RAISED AT " & "DECLARATION OF R3");
         exception
            when others =>
               Failed ("EXCEPTION FOR R3 RAISED INSIDE " & "BLOCK");
         end;

      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED AT DECLARATION " & "OF R3");
      end;

   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR TYPE " & "DECLARATION OF REC3");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR TYPE " & "DECLARATION OF REC3");
   end;

   begin
      declare
         package Lpriv is
            type Rec4 (D : Natural := Ident_Int (0)) is limited private;
            procedure Proc (R : Rec4);

         private
            type Rec4 (D : Natural := Ident_Int (0)) is record
               A : Arr (D .. 5);
            end record;
         end Lpriv;

         package body Lpriv is
            procedure Proc (R : Rec4) is
               I : Integer;
            begin
               I := Ident_Int (R.A'First);
               if Equal (2, Ident_Int (1)) then
                  Failed ("IMPOSSIBLE " & Integer'Image (I));  --USE I
               end if;
            end Proc;
         end Lpriv;

         use Lpriv;

      begin
         declare
            R4 : Rec4;

         begin
            Proc (R4);
            Failed ("NO EXCEPTION RAISED AT " & "DECLARATION OF R4");
         exception
            when others =>
               Failed ("EXCEPTION FOR R4 RAISED INSIDE " & "BLOCK");
         end;

      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED AT DECLARATION " & "OF R4");
      end;

   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR TYPE " & "DECLARATION OF REC4");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR TYPE " & "DECLARATION OF REC4");
   end;

   Result;
end C37108b;
