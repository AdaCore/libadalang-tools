-- CC3128A.ADA

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
--     CHECK THAT, FOR A CONSTRAINED IN FORMAL PARAMETER HAVING AN ACCESS TYPE,
--     CONSTRAINT_ERROR IS RAISED IF AND ONLY IF THE ACTUAL PARAMETER IS NOT
--     NULL AND THE OBJECT DESIGNATED BY THE ACTUAL PARAMETER DOES NOT SATISFY
--     THE FORMAL PARAMETER'S CONSTRAINTS.

-- HISTORY:
--     RJW 10/28/88  CREATED ORIGINAL TEST.
--     JRL 02/28/96  Removed cases where the designated subtypes of the formal
--                   and actual do not statically match. Corrected commentary.

with Report; use Report;
procedure Cc3128a is

begin
   Test
     ("CC3128A",
      "FOR A CONSTRAINED IN FORMAL PARAMETER HAVING " &
      "AN ACCESS TYPE, CONSTRAINT_ERROR IS RAISED " &
      "IF AND ONLY IF THE ACTUAL PARAMETER IS NOT " &
      "NULL AND THE OBJECT DESIGNATED BY THE ACTUAL " &
      "PARAMETER DOES NOT SATISFY FORMAL PARAMETER'S " & "CONSTRAINTS");

   declare
      type Rec (D : Integer := 10) is record
         null;
      end record;

      type Accrec is access Rec;

      subtype Link is Accrec (5);

      generic
         Link1 : Link;
      function F (I : Integer) return Integer;

      function F (I : Integer) return Integer is
      begin
         if I /= 5 then
            Failed
              ("CONSTRAINT_ERROR NOT RAISED PRIOR " &
               "TO CALL TO FUNCTION F - 1");
         end if;
         if not Equal (I, 5) and then not Equal (Link1.D, Link1.D) then
            Comment ("DISREGARD");
         end if;
         return I + 1;
      exception
         when others =>
            Failed ("EXCEPTION RAISED WITHIN FUNCTION F - 1");
            return I + 1;
      end F;

      generic
         type Priv (D : Integer) is private;
         Priv1 : Priv;
      package Gen is
         type Accpriv is access Priv;
         subtype Link is Accpriv (5);
         generic
            Link1 : Link;
            I : in out Integer;
         package P is
         end P;
      end Gen;

      package body Gen is
         package body P is
         begin
            if I /= 5 then
               Failed
                 ("CONSTRAINT_ERROR NOT RAISED PRIOR " &
                  "TO PACKAGE BODY P - 1");
            end if;
            if not Equal (I, 5) and then not Equal (Link1.D, Link1.D) then
               Comment ("DISREGARD");
            end if;
            I := I + 1;
         exception
            when others =>
               Failed ("EXCEPTION RAISED WITHIN " & "PACKAGE P - 1");
               I := I + 1;
         end P;

      begin
         begin
            declare
               Ar10 : Accpriv;
               I    : Integer := Ident_Int (5);
               package P1 is new P (Ar10, I);
            begin
               if I /= 6 then
                  Failed ("INCORRECT RESULT - " & "PACKAGE P1");
               end if;
            exception
               when others =>
                  Failed ("EXCEPTION RAISED TOO LATE - " & "PACKAGE P1 - 1");
            end;
         exception
            when others =>
               Failed
                 ("EXCEPTION RAISED AT INSTANTIATION " &
                  "OF PACKAGE P1 WITH NULL ACCESS " & "VALUE");
         end;

         begin
            declare
               Ar10 : Accpriv := new Priv'(Priv1);
               I    : Integer := Ident_Int (0);
               package P1 is new P (Ar10, I);
            begin
               Failed
                 ("NO EXCEPTION RAISED BY " & "INSTANTIATION OF PACKAGE P1");
            exception
               when others =>
                  Failed ("EXCEPTION RAISED TOO LATE - " & "PACKAGE P1 - 2");
            end;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED AT " &
                  "INSTANTIATION OF PACKAGE P1");
         end;
      end Gen;

      package Newgen is new Gen (Rec, (D => 10));

   begin
      begin
         declare
            I    : Integer := Ident_Int (5);
            Ar10 : Accrec;
            function F1 is new F (Ar10);
         begin
            I := F1 (I);
            if I /= 6 then
               Failed ("INCORRECT RESULT RETURNED BY " & "FUNCTION F1");
            end if;
         exception
            when others =>
               Failed ("EXCEPTION RAISED AT CALL TO " & "FUNCTION F1 - 1");
         end;
      exception
         when others =>
            Failed
              ("EXCEPTION RAISED AT INSTANTIATION OF " &
               "FUNCTION F1 WITH NULL ACCESS VALUE");
      end;

      begin
         declare
            I    : Integer := Ident_Int (0);
            Ar10 : Accrec  := new Rec'(D => 10);
            function F1 is new F (Ar10);
         begin
            Failed
              ("NO EXCEPTION RAISED BY INSTANTIATION " & "OF FUNCTION F1");
            I := F1 (I);
         exception
            when others =>
               Failed ("EXCEPTION RAISED AT CALL TO " & "FUNCTION F1 - 2");
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED AT " & "INSTANTIATION OF FUNCTION F1");
      end;
   end;

   declare
      type Arr is array (Positive range <>) of Integer;

      type Accarr is access Arr;

      subtype Link is Accarr (1 .. 5);

      generic
         Link1 : Link;
      function F (I : Integer) return Integer;

      function F (I : Integer) return Integer is
      begin
         if I /= 5 then
            Failed
              ("CONSTRAINT_ERROR NOT RAISED PRIOR " &
               "TO CALL TO FUNCTION F - 2");
         end if;
         if not Equal (I, 5)
           and then not Equal (Link1 (Ident_Int (3)), Link1 (Ident_Int (3)))
         then
            Comment ("DISREGARD");
         end if;
         return I + 1;
      exception
         when others =>
            Failed ("EXCEPTION RAISED WITHIN FUNCTION F - 2");
            return I + 1;
      end F;

      generic
         type Genarr is array (Positive range <>) of Integer;
      package Gen is
         type Accgenarr is access Genarr;
         subtype Link is Accgenarr (1 .. 5);
         generic
            Link1 : Link;
            I : in out Integer;
         package P is
         end P;
      end Gen;

      package body Gen is
         package body P is
         begin
            if I /= 5 then
               Failed
                 ("CONSTRAINT_ERROR NOT RAISED PRIOR " &
                  "TO PACKAGE BODY P - 2");
            end if;
            if not Equal (I, 5)
              and then not Equal (Link1 (Ident_Int (3)), Link1 (Ident_Int (3)))
            then
               Comment ("DISREGARD");
            end if;
            I := I + 1;
         exception
            when others =>
               Failed ("EXCEPTION RAISED WITHIN " & "PACKAGE P - 2");
               I := I + 1;
         end P;

      begin
         begin
            declare
               Ar26 : Accgenarr (2 .. 6);
               I    : Integer := Ident_Int (5);
               package P2 is new P (Ar26, I);
            begin
               if I /= 6 then
                  Failed ("INCORRECT RESULT - " & "PACKAGE P2");
               end if;
            exception
               when others =>
                  Failed ("EXCEPTION RAISED TOO LATE - " & "PACKAGE P2 - 1");
            end;
         exception
            when others =>
               Failed
                 ("EXCEPTION RAISED AT INSTANTIATION " &
                  "OF PACKAGE P2 WITH NULL ACCESS " & "VALUE");
         end;

         begin
            declare
               Ar26 : Accgenarr (Ident_Int (2) .. Ident_Int (6)) :=
                 new Genarr'(1, 2, 3, 4, 5);
               I : Integer := Ident_Int (0);
               package P2 is new P (Ar26, I);
            begin
               Failed
                 ("NO EXCEPTION RAISED BY " & "INSTANTIATION OF PACKAGE P2");
            exception
               when others =>
                  Failed ("EXCEPTION RAISED TOO LATE - " & "PACKAGE P2 - 2");
            end;
         exception
            when Constraint_Error =>
               null;
            when others =>
               Failed
                 ("WRONG EXCEPTION RAISED AT " &
                  "INSTANTIATION OF PACKAGE P2");
         end;
      end Gen;

      package Newgen is new Gen (Arr);

   begin
      begin
         declare
            I    : Integer := Ident_Int (5);
            Ar26 : Accarr (Ident_Int (2) .. Ident_Int (6));
            function F2 is new F (Ar26);
         begin
            I := F2 (I);
            if I /= 6 then
               Failed ("INCORRECT RESULT RETURNED BY " & "FUNCTION F2");
            end if;
         exception
            when others =>
               Failed ("EXCEPTION RAISED AT CALL TO " & "FUNCTION F2 - 1");
         end;
      exception
         when others =>
            Failed
              ("EXCEPTION RAISED AT INSTANTIATION OF " &
               "FUNCTION F2 WITH NULL ACCESS VALUE");
      end;

      begin
         declare
            I    : Integer         := Ident_Int (0);
            Ar26 : Accarr (2 .. 6) := new Arr'(1, 2, 3, 4, 5);
            function F2 is new F (Ar26);
         begin
            Failed
              ("NO EXCEPTION RAISED BY INSTANTIATION " & "OF FUNCTION F2");
            I := F2 (I);
         exception
            when others =>
               Failed ("EXCEPTION RAISED AT CALL TO " & "FUNCTION F2 - 2");
         end;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED AT " & "INSTANTIATION OF FUNCTION F2");
      end;
   end;
   Result;
end Cc3128a;
