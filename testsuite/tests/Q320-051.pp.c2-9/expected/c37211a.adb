-- C37211A.ADA

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
-- DISCRIMINANT. THIS TEST CONTAINS CHECKS FOR SUBTYPE INDICATIONS WHERE
-- THE TYPE MARK DENOTES A RECORD TYPE.

-- R.WILLIAMS 8/28/86
-- EDS 7/14/98 AVOID OPTIMIZATION

with Report; use Report;
procedure C37211a is

   type Rec (D : Positive) is record
      null;
   end record;

begin
   Test
     ("C37211A",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED BY " &
      "A DISCRIMINANT CONSTRAINT IF A VALUE " &
      "SPECIFIED FOR A DISCRIMINANT DOES NOT LIE " &
      "IN THE RANGE OF THE DISCRIMINANT WHERE THE " &
      "TYPE MARK DENOTES A RECORD TYPE");

   begin
      declare
         subtype Subrec is Rec (Ident_Int (-1));
      begin
         declare
            Sr : Subrec;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF SUBTYPE SUBREC " &
               Integer'Image (Sr.D));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT SR");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "SUBTYPE SUBREC");
   end;

   begin
      declare
         type Arr is array (1 .. 10) of Rec (Ident_Int (-1));
      begin
         declare
            Ar : Arr;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF TYPE ARR " &
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
            X : Rec (Ident_Int (-1));
         end record;

      begin
         declare
            R1 : Rec1;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF TYPE REC1 " &
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
         type Accrec is access Rec (Ident_Int (-1));
      begin
         declare
            Acr : Accrec;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF TYPE ACCREC " &
               Integer'Image (Acr.D));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT ACR");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE ACCREC");
   end;

   begin
      declare
         type Newrec is new Rec (Ident_Int (-1));
      begin
         declare
            Nr : Newrec;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "ELABORATION OF TYPE NEWREC " &
               Integer'Image (Nr.D));
         end;
      exception
         when others =>
            Failed ("EXCEPTION RAISED AT DECLARATION OF " & "OBJECT NR");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT ELABORATION OF " & "TYPE NEWREC");
   end;

   begin
      declare
         R : Rec (Ident_Int (-1));
      begin
         Failed
           ("NO EXCEPTION RAISED AT THE DECLARATION OF " &
            "R " &
            Integer'Image (R.D));
      exception
         when others =>
            Failed ("EXCEPTION RAISED INSIDE BLOCK " & "CONTAINING R");
      end;

   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION RAISED AT DECLARATION OF " & "R");
   end;

   begin
      declare
         type Rec_Name is access Rec;
      begin
         declare
            Rn : Rec_Name := new Rec (Ident_Int (-1));
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "DECLARATION OF OBJECT RN " &
               Integer'Image (Rn.D));
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED AT DECLARATION " & "OF OBJECT RN");
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED AT ELABORATION OF TYPE " & "REC_NAME");
   end;

   begin
      declare
         type Bad_Rec (D : Positive := Ident_Int (-1)) is record
            null;
         end record;
      begin
         declare
            Br : Bad_Rec;
         begin
            Failed
              ("NO EXCEPTION RAISED AT THE " &
               "DECLARATION OF OBJECT BR " &
               Integer'Image (Br.D));
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED AT DECLARATION " & "OF OBJECT BR");
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED AT ELABORATION OF TYPE " & "BAD_REC");
   end;

   Result;
end C37211a;
