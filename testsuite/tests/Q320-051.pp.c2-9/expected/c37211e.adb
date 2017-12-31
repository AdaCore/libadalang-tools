-- C37211E.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED BY A DISCRIMINANT CONSTRAINT IF
-- A VALUE SPECIFIED FOR A DISCRIMINANT DOES NOT LIE IN THE RANGE OF THE
-- DISCRIMINANT.

-- R.WILLIAMS 8/28/86
-- PWN 10/27/95 REMOVED CHECK WHERE CONSTRAINT RULES HAVE CHANGED. PWN 12/03/95
-- CORRECTED FORMATING PROBLEM. TMB 11/20/96 REINTRODUCED CHECK REMOVED ON
-- 10/27 WITH ADA95 CHANGES TMB 12/2/96 DELETED CHECK OF CONSTRAINED ACCESS
-- TYPE EDS 07/14/98 AVOID OPTIMIZATION

with Report; use Report;
procedure C37211e is

   type Rec (D : Positive) is record
      null;
   end record;

   type Acc is access Rec;
begin
   Test
     ("C37211E",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED BY " &
      "A DISCRIMINANT CONSTRAINT IF A VALUE " &
      "SPECIFIED FOR A DISCRIMINANT DOES NOT LIE " &
      "IN THE RANGE OF THE DISCRIMINANT WHERE THE " &
      "TYPE MARK DENOTES AN ACCESS TYPE");

   begin
      declare
         subtype Subacc is Acc (Ident_Int (-1));
      begin
         declare
            Sa : Subacc;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF SUBTYPE SUBACC " & Integer'Image (Sa.D));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT SA");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "SUBTYPE SUBACC");
   end;

   begin
      declare
         type Arr is array (1 .. 10) of Acc (Ident_Int (-1));
      begin
         declare
            Ar : Arr;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " & "ELABORATION OF TYPE ARR " &
               Integer'Image (Ar (1).D));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT AR");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE ARR");
   end;

   begin
      declare
         type Rec1 is record
            X : Acc (Ident_Int (-1));
         end record;

      begin
         declare
            R1 : Rec1;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " & "ELABORATION OF TYPE REC1 " &
               Integer'Image (R1.X.D));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT R1");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE REC1");
   end;

   begin
      declare
         type Acca is access Acc (Ident_Int (-1));
      begin
         declare
            Aca : Acca;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " & "ELABORATION OF TYPE ACCA " &
               Integer'Image (Aca.all.D));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT ACA");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE ACCA");
   end;

   begin
      declare
         type Newacc is new Acc (Ident_Int (-1));
      begin
         declare
            Na : Newacc;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " & "ELABORATION OF TYPE NEWACC " &
               Integer'Image (Na.D));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT NA");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE NEWACC");
   end;

   begin
      declare
         A : Acc (Ident_Int (-1));
      begin
         Failed
           ("NO EXCEPTION RAISED AT THE DECLARATION OF " & "A " &
            Integer'Image (A.D));
      exception
         when others =>
            Failed ("EXCEPTION RAISED INSIDE BLOCK " & "CONTAINING A");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT DECLARATION OF " & "A");
   end;

   begin
      declare
         type Bad_Acc (D : Positive := Ident_Int (-1)) is record
            null;
         end record;
      begin
         declare
            Bac : Bad_Acc;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " & "DECLARATION OF OBJECT BAC " &
               Integer'Image (Bac.D));
         exception
            when others =>
               Failed ("EXCEPTION RAISED INSIDE BLOCK " & "DECLARING BAC");
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED AT DECLARATION " & "OF OBJECT BAC");
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED AT ELABORATION OF TYPE " & "BAD_ACC");
   end;

   Result;
end C37211e;
