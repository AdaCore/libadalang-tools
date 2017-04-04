-- C32115B.ADA

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
--    CHECK THAT WHEN A VARIABLE OR CONSTANT HAVING AN UNCONSTRAINED
--    ACCESS TYPE IS DECLARED WITH AN INITIAL NON-NULL ACCESS VALUE,
--    CONSTRAINT_ERROR IS RAISED IF AN INDEX BOUND OR A DISCRIMINANT
--    VALUE OF THE DESIGNATED OBJECT DOES NOT EQUAL THE CORRESPONDING
--    VALUE SPECIFIED FOR THE ACCESS SUBTYPE OF THE OBJECT.

-- HISTORY:
--    JET 08/05/87  CREATED ORIGINAL TEST BASED ON C32115A BY RJW
--                  BUT WITH UNCONSTRAINED ACCESS TYPES AND
--                  CONSTRAINED VARIABLE/CONSTANT DECLARATIONS.
-- KAS 12/4/95 FIXED TYPO IN CALL TO REPORT.TEST

with Report; use Report;

procedure C32115b is

   package Pkg is
      type Priv (D : Integer) is private;

   private
      type Priv (D : Integer) is record
         null;
      end record;
   end Pkg;

   use Pkg;

   type Accp is access Priv;

   type Rec (D : Integer) is record
      null;
   end record;

   type Accr is access Rec;

   type Arr is array (Natural range <>) of Integer;

   type Acca is access Arr;

   type Accn is access Arr;

begin
   Test
     ("C32115B",
      "CHECK THAT WHEN CONSTRAINED VARIABLE OR " &
      "CONSTANT HAVING AN UNCONSTRAINED ACCESS TYPE " &
      "IS DECLARED WITH AN INITIAL NON-NULL ACCESS " &
      "VALUE, CONSTRAINT_ERROR IS RAISED IF AN " &
      "INDEX BOUND OR A DISCRIMINANT VALUE OF THE " &
      "DESIGNATED OBJECT DOES NOT EQUAL THE " &
      "CORRESPONDING VALUE SPECIFIED FOR THE " &
      "ACCESS SUBTYPE OF THE OBJECT");

   begin
      declare
         Ac1 : constant Accp (1) := new Priv (Ident_Int (2));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'AC1'");
         if Ac1 /= null then
            Comment ("DEFEAT 'AC1' OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'AC1'");
   end;

   begin
      declare
         Ac2 : Accp (1) := new Priv (Ident_Int (2));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'AC2'");
         if Ac2 /= null then
            Comment ("DEFEAT 'AC2' OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'AC2'");
   end;

   begin
      declare
         Ac3 : constant Accp (1) := new Priv (Ident_Int (0));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'AC3'");
         if Ac3 /= null then
            Comment ("DEFEAT 'AC3' OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'AC3'");
   end;

   begin
      declare
         Ac4 : Accp (1) := new Priv (Ident_Int (0));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'AC4'");
         if Ac4 /= null then
            Comment ("DEFEAT 'AC4' OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'AC4'");
   end;

   begin
      declare
         Ac5 : constant Accr (2) := new Rec (Ident_Int (1));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'AC5'");
         if Ac5 /= null then
            Comment ("DEFEAT 'AC5' OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'AC5'");
   end;

   begin
      declare
         Ac6 : Accr (2) := new Rec (Ident_Int (1));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'AC6'");
         if Ac6 /= null then
            Comment ("DEFEAT 'AC6' OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'AC6'");
   end;

   begin
      declare
         Ac7 : constant Accr (2) := new Rec (Ident_Int (3));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'AC7'");
         if Ac7 /= null then
            Comment ("DEFEAT 'AC7' OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'AC7'");
   end;

   begin
      declare
         Ac8 : Accr (2) := new Rec (Ident_Int (3));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'AC8'");
         if Ac8 /= null then
            Comment ("DEFEAT 'AC8' OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'AC8'");
   end;

   begin
      declare
         Ac9 : constant Acca (1 .. 2) :=
           new Arr (Ident_Int (1) .. Ident_Int (1));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'AC9'");
         if Ac9 /= null then
            Comment ("DEFEAT 'AC9' OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'AC9'");
   end;

   begin
      declare
         Ac10 : Acca (1 .. 2) := new Arr (Ident_Int (1) .. Ident_Int (1));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'AC10'");
         if Ac10 /= null then
            Comment ("DEFEAT 'AC10' OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'AC10'");
   end;

   begin
      declare
         Ac11 : constant Acca (1 .. 2) :=
           new Arr (Ident_Int (0) .. Ident_Int (2));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'AC11'");
         if Ac11 /= null then
            Comment ("DEFEAT 'AC11' OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'AC11'");
   end;

   begin
      declare
         Ac12 : Acca (1 .. 2) := new Arr (Ident_Int (0) .. Ident_Int (2));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'AC12'");
         if Ac12 /= null then
            Comment ("DEFEAT 'AC12' OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'AC12'");
   end;

   begin
      declare
         Ac13 : constant Acca (1 .. 2) :=
           new Arr (Ident_Int (2) .. Ident_Int (3));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'AC13'");
         if Ac13 /= null then
            Comment ("DEFEAT 'AC13' OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'AC13'");
   end;

   begin
      declare
         Ac14 : Acca (1 .. 2) := new Arr (Ident_Int (2) .. Ident_Int (3));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'AC14'");
         if Ac14 /= null then
            Comment ("DEFEAT 'AC14' OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'AC14'");
   end;

   begin
      declare
         Ac15 : constant Accn (1 .. 0) :=
           new Arr (Ident_Int (0) .. Ident_Int (0));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF CONSTANT 'AC15'");
         if Ac15 /= null then
            Comment ("DEFEAT 'AC15' OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF CONSTANT 'AC15'");
   end;

   begin
      declare
         Ac16 : Accn (1 .. 0) := new Arr (Ident_Int (0) .. Ident_Int (0));
      begin
         Failed
           ("NO EXCEPTION RAISED FOR INITIALIZATION " & "OF VARIABLE 'AC16'");
         if Ac16 /= null then
            Comment ("DEFEAT 'AC16' OPTIMIZATION");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
            "OF VARIABLE 'AC16'");
   end;

   Result;
end C32115b;
