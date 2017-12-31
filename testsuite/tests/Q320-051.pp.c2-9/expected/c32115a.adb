-- C32115A.ADA

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
--    CHECK THAT WHEN A VARIABLE OR CONSTANT HAVING A CONSTRAINED
--    ACCESS TYPE IS DECLARED WITH AN INITIAL NON-NULL ACCESS VALUE,
--    CONSTRAINT_ERROR IS RAISED IF AN INDEX BOUND OR A DISCRIMINANT
--    VALUE OF THE DESIGNATED OBJECT DOES NOT EQUAL THE CORRESPONDING
--    VALUE SPECIFIED FOR THE ACCESS SUBTYPE.

-- HISTORY:
--    RJW 07/20/86  CREATED ORIGINAL TEST.
--    JET 08/05/87  ADDED DEFEAT OF DEAD VARIABLE OPTIMIZATION.
--    PWN 11/30/94  REMOVED TEST ILLEGAL IN ADA 9X.

with Report; use Report;

procedure C32115a is

   package Pkg is
      type Priv (D : Integer) is private;

   private
      type Priv (D : Integer) is record
         null;
      end record;
   end Pkg;

   use Pkg;

   type Accp is access Priv (Ident_Int (1));

   type Rec (D : Integer) is record
      null;
   end record;

   type Accr is access Rec (Ident_Int (2));

   type Arr is array (Natural range <>) of Integer;

   type Acca is access Arr (Ident_Int (1) .. Ident_Int (2));

   type Accn is access Arr (Ident_Int (1) .. Ident_Int (0));

begin
   Test
     ("C32115A",
      "CHECK THAT WHEN A VARIABLE OR CONSTANT " &
      "HAVING A CONSTRAINED ACCESS TYPE IS " &
      "DECLARED WITH AN INITIAL NON-NULL ACCESS " &
      "VALUE, CONSTRAINT_ERROR IS RAISED IF AN " &
      "INDEX BOUND OR A DISCRIMINANT VALUE OF THE " &
      "DESIGNATED OBJECT DOES NOT EQUAL THE " &
      "CORRESPONDING VALUE SPECIFIED FOR THE " & "ACCESS SUBTYPE");

   begin
      declare
         Ac1 : constant Accp := new Priv (D => (Ident_Int (2)));
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
         Ac2 : Accp := new Priv (D => (Ident_Int (2)));
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
         Ac3 : constant Accp := new Priv (D => (Ident_Int (0)));
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
         Ac4 : Accp := new Priv (D => (Ident_Int (0)));
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
         Ac5 : constant Accr := new Rec'(D => (Ident_Int (1)));
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
         Ac6 : Accr := new Rec'(D => (Ident_Int (1)));
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
         Ac7 : constant Accr := new Rec'(D => (Ident_Int (3)));
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
         Ac8 : Accr := new Rec'(D => (Ident_Int (3)));
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
         Ac9 : constant Acca := new Arr'(Ident_Int (1) .. Ident_Int (1) => 0);
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
         Ac10 : Acca := new Arr'(Ident_Int (1) .. Ident_Int (1) => 0);
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
         Ac11 : constant Acca := new Arr'(Ident_Int (0) .. Ident_Int (2) => 0);
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
         Ac12 : Acca := new Arr'(Ident_Int (0) .. Ident_Int (2) => 0);
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
         Ac15 : constant Accn := new Arr'(Ident_Int (0) .. Ident_Int (0) => 0);
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
         Ac16 : Accn := new Arr'(Ident_Int (0) .. Ident_Int (0) => 0);
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
end C32115a;
