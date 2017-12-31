-- C47008A.ADA

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
--     WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION DENOTES A
--     CONSTRAINED RECORD, PRIVATE, OR LIMITED PRIVATE TYPE, CHECK THAT
--     CONSTRAINT_ERROR IS RAISED WHEN THE DISCRIMINANTS OF THE OPERAND
--     DO NOT EQUAL THOSE OF THE TYPE MARK.

-- HISTORY:
--     RJW 07/23/86
--     DWC 07/24/87  CHANGED CODE TO TEST FOR FIRST DISCRIMINANT
--                   AND LAST DISCRIMINANT MISMATCH.

with Report; use Report;
procedure C47008a is

   type Gender is (Male, Female, Neuter);

   function Ident (G : Gender) return Gender is
   begin
      return Gender'Val (Ident_Int (Gender'Pos (G)));
   end Ident;

begin

   Test
     ("C47008A",
      "WHEN THE TYPE MARK IN A QUALIFIED " &
      "EXPRESSION DENOTES A CONSTRAINED RECORD, " &
      "PRIVATE, OR LIMITED PRIVATE TYPE, CHECK " &
      "THAT CONSTRAINT_ERROR IS RAISED WHEN THE " &
      "DISCRIMANTS OF THE OPERAND DO NOT EQUAL " & "THOSE OF THE TYPE MARK");

   declare

      type Person (Sex : Gender) is record
         null;
      end record;

      subtype Woman is Person (Ident (Female));
      Tom : Person (Male) := (Sex => Ident (Male));

   begin
      if Woman'(Tom) = Person'(Sex => Male) then
         Failed
           ("NO EXCEPTION RAISED FOR OPERAND WITH DISC " &
            "NOT EQUAL TO THOSE OF SUBTYPE WOMAN - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR OPERAND WITH DISC " &
            "NOT EQUAL TO THOSE OF SUBTYPE WOMAN - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR OPERAND WITH " &
            "DISC NOT EQUAL TO THOSE OF SUBTYPE WOMAN");
   end;

   declare
      type Pair (Sex1, Sex2 : Gender) is record
         null;
      end record;

      subtype Couple is Pair (Ident (Female), Ident (Male));
      Joneses : Pair (Ident (Male), Ident (Female));

   begin
      if Couple'(Joneses) = Pair'(Sex1 => Male, Sex2 => Female) then
         Failed
           ("NO EXCEPTION RAISED FOR OPERAND WITH DISC " &
            "NOT EQUAL TO THOSE OF SUBTYPE COUPLE - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR OPERAND WITH DISC " &
            "NOT EQUAL TO THOSE OF SUBTYPE COUPLE - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR OPERAND WITH " &
            "DISC NOT EQUAL TO THOSE OF SUBTYPE COUPLE");
   end;

   declare

      package Pkg is
         type Person (Sex : Gender) is private;
         subtype Man is Person (Ident (Male));

         Testwriter : constant Person;

      private
         type Person (Sex : Gender) is record
            null;
         end record;

         Testwriter : constant Person := (Sex => Female);

      end Pkg;

      use Pkg;

      Rosa : Person (Ident (Female));

   begin
      if Man'(Rosa) = Testwriter then
         Failed
           ("NO EXCEPTION RAISED FOR OPERAND WITH DISC " &
            "NOT EQUAL TO THOSE OF SUBTYPE MAN - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR OPERAND WITH DISC " &
            "NOT EQUAL TO THOSE OF SUBTYPE MAN - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR OPERAND WITH " &
            "DISC NOT EQUAL TO THOSE OF SUBTYPE MAN");
   end;

   declare
      package Pkg is
         type Pair (Sex1, Sex2 : Gender) is private;
         subtype Friends is Pair (Ident (Female), Ident (Male));

         Alice_And_Jerry : constant Friends;

      private
         type Pair (Sex1, Sex2 : Gender) is record
            null;
         end record;

         Alice_And_Jerry : constant Friends := (Ident (Female), Ident (Male));

      end Pkg;

      use Pkg;

      Dick_And_Joe : Pair (Ident (Male), Ident (Male));

   begin
      if Friends'(Dick_And_Joe) = Alice_And_Jerry then
         Failed
           ("NO EXCEPTION RAISED FOR OPERAND WITH DISC " &
            "NOT EQUAL TO THOSE OF SUBTYPE FRIENDS - 1");
      else
         Failed
           ("NO EXCEPTION RAISED FOR OPERAND WITH DISC " &
            "NOT EQUAL TO THOSE OF SUBTYPE FRIENDS - 2");
      end if;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed
           ("WRONG EXCEPTION RAISED FOR OPERAND WITH " &
            "DISC NOT EQUAL TO THOSE OF SUBTYPE FRIENDS");
   end;

   declare

      package Pkg1 is
         type Person (Sex : Gender) is limited private;
         subtype Android is Person (Ident (Neuter));

         function F return Person;
         function "=" (A, B : Person) return Boolean;
      private
         type Person (Sex : Gender) is record
            null;
         end record;

      end Pkg1;

      package body Pkg1 is

         function F return Person is
         begin
            return Person'(Sex => (Ident (Male)));
         end F;

         function "=" (A, B : Person) return Boolean is
         begin
            return A.Sex = B.Sex;
         end "=";

      end Pkg1;

      package Pkg2 is
      end Pkg2;

      package body Pkg2 is
         use Pkg1;

      begin
         if Android'(F) = F then
            Failed
              ("NO EXCEPTION RAISED FOR OPERAND WITH " &
               "DISC NOT EQUAL TO THOSE OF SUBTYPE " & "ANDROID - 1");
         else
            Failed
              ("NO EXCEPTION RAISED FOR OPERAND WITH " &
               "DISC NOT EQUAL TO THOSE OF SUBTYPE " & "ANDROID - 2");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED FOR OPERAND " &
               "WITH DISC NOT EQUAL TO THOSE OF " & "SUBTYPE ANDROID");
      end Pkg2;

   begin
      null;
   end;

   declare
      package Pkg1 is
         type Pair (Sex1, Sex2 : Gender) is limited private;
         subtype Lovers is Pair (Ident (Female), Ident (Male));

         function F return Pair;
         function "=" (A, B : Pair) return Boolean;
      private
         type Pair (Sex1, Sex2 : Gender) is record
            null;
         end record;
      end Pkg1;

      package body Pkg1 is

         function F return Pair is
         begin
            return Pair'(Sex1 => (Ident (Female)), Sex2 => (Ident (Female)));
         end F;

         function "=" (A, B : Pair) return Boolean is
         begin
            return A.Sex1 = B.Sex2;
         end "=";

      end Pkg1;

      package Pkg2 is
      end Pkg2;

      package body Pkg2 is
         use Pkg1;

      begin
         if Lovers'(F) = F then
            Failed
              ("NO EXCEPTION RAISED FOR OPERAND WITH " &
               "DISC NOT EQUAL TO THOSE OF SUBTYPE " & "LOVERS - 1");
         else
            Failed
              ("NO EXCEPTION RAISED FOR OPERAND WITH " &
               "DISC NOT EQUAL TO THOSE OF SUBTYPE " & "LOVERS - 2");
         end if;
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed
              ("WRONG EXCEPTION RAISED FOR OPERAND " &
               "WITH DISC NOT EQUAL TO THOSE OF " & "SUBTYPE LOVERS");
      end Pkg2;

   begin
      null;
   end;

   Result;
end C47008a;
