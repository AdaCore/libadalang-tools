-- C43103A.ADA

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
-- CHECK THAT IF A DISCRIMINANT DOES NOT GOVERN A VARIANT PART,
-- ITS VALUE CAN BE GIVEN BY A NON-STATIC EXPRESSION.

-- EG  02/13/84

with Report;

procedure C43103a is

   use Report;

begin

   Test
     ("C43103A",
      "CHECK THAT IF A DISCRIMINANT DOES NOT GOVERN " &
      "A VARIANT PART, ITS VALUE CAN BE GIVEN BY A " &
      "NON-STATIC EXPRESSION");

   begin

      Comment
        ("CASE A : DISCRIMINANT THAT IS NOT USED INSIDE " & "THE RECORD");

      Case_A : declare

         type R1 (A : Integer) is record
            B : String (1 .. 2);
            C : Integer;
         end record;

         A1 : R1 (Ident_Int (5)) := (Ident_Int (5), "AB", -2);

      begin

         if A1.A /= Ident_Int (5) or A1.B /= "AB" or A1.C /= -2 then
            Failed ("CASE A : INCORRECT VALUES IN RECORD");
         end if;

      end Case_A;

      Comment
        ("CASE B : DISCRIMINANT THAT IS USED AS AN ARRAY " & "INDEX BOUND");

      Case_B : declare

         subtype Stb is Integer range 1 .. 10;
         type Tb is array (Stb range <>) of Integer;
         type R2 (A : Stb) is record
            B : Tb (1 .. A);
            C : Boolean;
         end record;

         B1 : R2 (Ident_Int (2)) := (Ident_Int (2), (-1, -2), False);

      begin

         if B1.B'Last /= Ident_Int (2) then
            Failed ("CASE B : INCORRECT UPPER BOUND");
         elsif B1.A /= Ident_Int (2) or B1.B /= (-1, -2) or B1.C /= False then
            Failed ("CASE B : INCORRECT VALUES IN RECORD");
         end if;

      end Case_B;

      Comment
        ("CASE C : DISCRIMINANT THAT IS USED IN A " &
         "DISCRIMINANT CONSTRAINT");

      Case_C : declare

         subtype Stc is Integer range 1 .. 10;
         type Tc is array (Stc range <>) of Integer;
         type R3 (A : Stc) is record
            B : Tc (1 .. A);
            C : Integer := -4;
         end record;
         type R4 (A : Integer) is record
            B : R3 (A);
            C : Integer;
         end record;

         C1 : R4 (Ident_Int (3)) :=
           (Ident_Int (3), (Ident_Int (3), (1, 2, 3), 4), 5);

      begin

         if C1.B.B /= (1, 2, 3) or C1.B.C /= 4 or C1.C /= 5 then
            Failed ("CASE C : INCORRECT VALUES IN RECORD");
         end if;

      end Case_C;

   end;

   Result;

end C43103a;
