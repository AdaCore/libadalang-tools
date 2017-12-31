-- C47009B.ADA

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
--     WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION DENOTES AN ACCESS
--     TYPE, CHECK THAT CONSTRAINT_ERROR IS NOT RAISED WHEN THE VALUE
--     OF THE OPERAND IS NULL.

-- HISTORY:
--     RJW 07/23/86  CREATED ORIGINAL TEST.
--     BCB 08/18/87  CHANGED HEADER TO STANDARD HEADER FORMAT.  CHANGED
--                   CONSTRAINTS OF B SUBTYPES TO VALUES WHICH ARE
--                   CLOSER TO THE VALUES OF THE A SUBTYPES.  INDENTED
--                   THE EXCEPTION STATEMENTS IN SUBTEST 11.

with Report; use Report;
procedure C47009b is

begin

   Test
     ("C47009B",
      "WHEN THE TYPE MARK IN A QUALIFIED " &
      "EXPRESSION DENOTES AN ACCESS TYPE, " &
      "CHECK THAT CONSTRAINT_ERROR IS NOT " &
      "RAISED WHEN THE VALUE OF THE OPERAND IS NULL");

   declare

      type Acc1 is access Boolean;
      A : Acc1;

   begin
      A := Acc1'(null);
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR TYPE ACC1");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR TYPE ACC1");
   end;

   declare

      type Acc2 is access Integer;
      A : Acc2;

   begin
      A := Acc2'(null);
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR TYPE ACC2");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR TYPE ACC2");
   end;

   declare

      type Char is ('A', 'B');
      type Acc3 is access Char;
      A : Acc3;

   begin
      A := Acc3'(null);
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR TYPE ACC3");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR TYPE ACC3");
   end;

   declare

      type Float1 is digits 5 range -1.0 .. 1.0;
      type Acc4 is access Float1;
      A : Acc4;

   begin
      A := Acc4'(null);
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR TYPE ACC4");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR TYPE ACC4");
   end;

   declare

      type Fixed is delta 0.5 range -1.0 .. 1.0;
      type Acc5 is access Fixed;
      A : Acc5;

   begin
      A := Acc5'(null);
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR TYPE ACC5");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR TYPE ACC5");
   end;

   declare

      type Arr is array (Natural range <>) of Integer;
      type Acc6 is access Arr;
      subtype Acc6a is Acc6 (Ident_Int (1) .. Ident_Int (5));
      subtype Acc6b is Acc6 (Ident_Int (2) .. Ident_Int (10));
      A : Acc6a;
      B : Acc6b;

   begin
      A := Acc6a'(B);
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR SUBTYPES OF " & "TYPE ACC6");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR SUBTYPES OF " & "TYPE ACC6");
   end;

   declare

      type Arr is array (Natural range <>, Natural range <>) of Integer;
      type Acc7 is access Arr;
      subtype Acc7a is
        Acc7 (Ident_Int (1) .. Ident_Int (5), Ident_Int (1) .. Ident_Int (1));
      subtype Acc7b is
        Acc7 (Ident_Int (1) .. Ident_Int (15),
           Ident_Int (1) .. Ident_Int (10));
      A : Acc7a;
      B : Acc7b;

   begin
      A := Acc7a'(B);
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR SUBTYPES OF " & "TYPE ACC7");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR SUBTYPES OF " & "TYPE ACC7");
   end;

   declare

      type Rec (D : Integer) is record
         null;
      end record;

      type Acc8 is access Rec;
      subtype Acc8a is Acc8 (Ident_Int (5));
      subtype Acc8b is Acc8 (Ident_Int (6));
      A : Acc8a;
      B : Acc8b;

   begin
      A := Acc8a'(B);
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR SUBTYPES OF " & "TYPE ACC8");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR SUBTYPES OF " & "TYPE ACC8");
   end;

   declare

      type Rec (D1, D2 : Integer) is record
         null;
      end record;

      type Acc9 is access Rec;
      subtype Acc9a is Acc9 (Ident_Int (4), Ident_Int (5));
      subtype Acc9b is Acc9 (Ident_Int (5), Ident_Int (4));
      A : Acc9a;
      B : Acc9b;

   begin
      A := Acc9a'(B);
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR SUBTYPES OF " & "TYPE ACC9");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR SUBTYPES OF " & "TYPE ACC9");
   end;

   declare

      package Pkg is
         type Rec (D : Integer) is private;

      private
         type Rec (D : Integer) is record
            null;
         end record;

      end Pkg;

      use Pkg;

      type Acc10 is access Rec;
      subtype Acc10a is Acc10 (Ident_Int (10));
      subtype Acc10b is Acc10 (Ident_Int (9));
      A : Acc10a;
      B : Acc10b;

   begin
      A := Acc10a'(B);
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR RAISED FOR SUBTYPES OF " & "TYPE ACC10");
      when others =>
         Failed ("OTHER EXCEPTION RAISED FOR SUBTYPES OF " & "TYPE ACC10");
   end;

   declare

      package Pkg1 is
         type Rec (D : Integer) is limited private;

      private
         type Rec (D : Integer) is record
            null;
         end record;
      end Pkg1;

      package Pkg2 is
      end Pkg2;

      package body Pkg2 is
         use Pkg1;

         type Acc11 is access Rec;
         subtype Acc11a is Acc11 (Ident_Int (11));
         subtype Acc11b is Acc11 (Ident_Int (12));
         A : Acc11a;
         B : Acc11b;

      begin
         A := Acc11a'(B);
      exception
         when Constraint_Error =>
            Failed ("CONSTRAINT_ERROR RAISED FOR SUBTYPES OF" & " TYPE ACC11");
         when others =>
            Failed ("OTHER EXCEPTION RAISED FOR SUBTYPES OF " & "TYPE ACC11");
      end Pkg2;

   begin
      null;
   end;

   Result;
end C47009b;
