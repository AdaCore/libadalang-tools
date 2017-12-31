-- C38107B.ADA

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
--     IF A DISCRIMINANT CONSTRAINT IS APPLIED TO AN ACCESS TYPE WHICH
--     DESIGNATES AN INCOMPLETE TYPE WHICH WAS DECLARED IN THE VISIBLE
--     OR PRIVATE PART OF A PACKAGE SPECIFICATION, OR IN A DECLARATIVE
--     PART, CONSTRAINT_ERROR IS RAISED IF ONE OF THE
--     DISCRIMINANT'S VALUES DOES NOT BELONG TO THE CORRESPONDING
--     DISCRIMINANT'S SUBTYPE.

-- HISTORY:
--     DHH 08/05/88 CREATED ORIGINAL TEST.

with Report; use Report;
procedure C38107b is

begin
   Test
     ("C38107B",
      "IF A DISCRIMINANT CONSTRAINT IS APPLIED TO AN " &
      "ACCESS TYPE WHICH DESIGNATES AN INCOMPLETE " &
      "TYPE WHICH WAS DECLARED IN THE VISIBLE OR " &
      "PRIVATE PART OF A PACKAGE SPECIFICATION, OR IN " &
      "A DECLARATIVE PART, CONSTRAINT_ERROR IS " &
      "RAISED IF ONE OF THE DISCRIMINANT'S VALUES " &
      "DOES NOT BELONG TO THE CORRESPONDING " & "DISCRIMINANT'S SUBTYPE");

------------------------------ VISIBLE ------------------------------
   begin
      declare
         package Pack is
            subtype Smaller is Integer range 1 .. 5;

            type Incomplete (A : Smaller);

            type Acc_Inc is access Incomplete;
            subtype Sub_Acc is Acc_Inc (Ident_Int (6));

            type Incomplete (A : Smaller) is record
               T : Integer := A;
            end record;

         end Pack;

         package body Pack is
         begin
            Failed ("CONSTRAINT_ERROR NOT RAISED - VISIBLE");
            declare
               Z : Sub_Acc := new Incomplete (Ident_Int (6));
            begin
               if Ident_Int (Z.T) = Ident_Int (6) then
                  Comment ("THIS LINE SHOULD NOT PRINT");
               end if;
            end;
         exception
            when Constraint_Error =>
               Failed ("CONSTRAINT_ERROR RAISED LATE " & "- VISIBLE");
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED " & "LATE - VISIBLE");
         end Pack;
      begin
         null;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED " & "- VISIBLE");
   end;

------------------------------ PRIVATE ------------------------------
   begin
      declare
         package Pack2 is
            subtype Smaller is Integer range 1 .. 5;

            type Priv is private;

         private
            type Priv is record
               V : Integer;
            end record;

            type Incomplete (A : Smaller);

            type Acc_Inc is access Incomplete;
            subtype Sub_Acc is Acc_Inc (Ident_Int (0));

            type Incomplete (A : Smaller) is record
               T : Integer := A;
               U : Priv    := (V => A**Ident_Int (2));
            end record;

         end Pack2;

         package body Pack2 is
         begin
            Failed ("CONSTRAINT_ERROR NOT RAISED - PRIVATE");
            declare
               Z : Sub_Acc := new Incomplete (Ident_Int (0));
            begin
               if Ident_Int (Z.T) = Ident_Int (0) then
                  Comment ("THIS LINE SHOULD NOT PRINT");
               end if;
            end;
         exception
            when Constraint_Error =>
               Failed ("CONSTRAINT_ERROR RAISED TOO LATE " & "- PRIVATE");
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED LATE" & "- PRIVATE");
         end Pack2;
      begin
         null;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED " & "- PRIVATE");
   end;

-------------------------- DECLARATIVE PART --------------------------
   begin
      declare
         subtype Smaller is Integer range 1 .. 5;

         type Incomplete (A : Smaller);

         type Acc_Inc is access Incomplete;
         subtype Sub_Acc is Acc_Inc (Ident_Int (6));

         type Incomplete (A : Smaller) is record
            T : Integer := Integer'(A);
         end record;

      begin
         Failed ("CONSTRAINT_ERROR NOT RAISED - BLOCK " & "STATEMENT");
         declare
            Z : Sub_Acc := new Incomplete (Ident_Int (6));
         begin
            if Ident_Int (Z.T) = Ident_Int (6) then
               Comment ("THIS LINE SHOULD NOT PRINT");
            end if;
         end;
      exception
         when Constraint_Error =>
            Failed ("CONSTRAINT_ERROR RAISED TOO LATE " & "- BLOCK STATEMENT");
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED LATE" & "- BLOCK STATEMENT");
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED " & "- BLOCK STATEMENT");
   end;

   Result;
end C38107b;
