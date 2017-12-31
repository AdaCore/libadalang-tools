-- C48005A.ADA

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
-- CHECK THAT AN ALLOCATOR OF THE FORM "NEW T X" ALLOCATES A NEW OBJECT EACH
-- TIME IT IS EXECUTED AND THAT IF T IS AN UNCONSTRAINED RECORD, PRIVATE, OR
-- LIMITED TYPE, THE ALLOCATED OBJECT HAS THE DISCRIMINANT VALUES SPECIFIED
-- BY X.

-- EG  08/08/84

with Report;

procedure C48005a is

   use Report;

begin

   Test
     ("C48005A",
      "CHECK THAT THE FORM 'NEW T X' ALLOCATES A " &
      "NEW OBJECT AND THAT IF T IS AN UNCONSTRAINED " &
      "RECORD, PRIVATE, OR LIMITED TYPE, THE " &
      "ALLOCATED OBJECT HAS THE DISCRIMINANT " & "VALUES SPECIFIED BY X");

   declare

      type Ur1 (A : Integer) is record
         B : Integer := 7;
         C : Integer := 4;
      end record;
      type Ur2 (A : Integer) is record
         case A is
            when 1 =>
               A1 : Integer := 4;
            when 2 =>
               A2 : Integer := 5;
            when others =>
               null;
         end case;
      end record;

      type A_Ur1 is access Ur1;
      type A_Ur2 is access Ur2;

      V1aur1         : A_Ur1;
      V1aur2, V2aur2 : A_Ur2;

      type Rec (A : Integer) is record
         B : Integer;
      end record;

      type A_Rec is access Rec;

      V_A_Rec : A_Rec;

      type Arr is array (1 .. 1) of Integer;

      type Recval is record
         A : Integer;
         B : Arr;
      end record;

      function Fun (A : Integer) return Integer is
      begin
         return Ident_Int (A);
      end Fun;
      function Fun (A : Integer) return Recval is
      begin
         Failed ("WRONG OVERLOADED FUNCTION CALLED");
         return (1, (1 => 2));
      end Fun;

   begin

      V1aur1 := new Ur1 (3);
      if (V1aur1.A /= 3 or V1aur1.B /= 7 or V1aur1.C /= Ident_Int (4)) then
         Failed ("WRONG VALUES - V1UAR1");
      end if;

      V1aur2 := new Ur2 (Ident_Int (2));
      if (V1aur2.A /= 2 or V1aur2.A2 /= Ident_Int (5)) then
         Failed ("WRONG VALUES - V1AUR2");
      end if;

      V2aur2 := new Ur2 (Ident_Int (3));
      if (V2aur2.A /= Ident_Int (3)) then
         Failed ("WRONG VALUES - V2AUR2");
      end if;

      V_A_Rec := new Rec (Fun (2));
   end;

   Result;

end C48005a;
