-- C54A13B.ADA

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
--     CHECK THAT IF A CASE EXPRESSION IS A GENERIC "IN" OR "IN OUT"
--     PARAMETER WITH A NON-STATIC SUBTYPE OR ONE OF THESE IN
--     PARENTHESES, THEN ANY VALUE OF THE EXPRESSION'S BASE TYPE MAY
--     APPEAR AS A CHOICE.

-- HISTORY:
--     BCB 07/13/88  CREATED ORIGINAL TEST.

with Report; use Report;

procedure C54a13b is

   L : Integer := Ident_Int (1);
   R : Integer := Ident_Int (100);

   subtype Int is Integer range L .. R;

   generic
      In_Par : in Int;
      In_Out_Par : in out Int;
   procedure Gen_Proc (I : in out Integer);

   In_Var     : Int := Ident_Int (10);
   In_Out_Var : Int := Ident_Int (100);
   Check_Var  : Int := Ident_Int (1);

   procedure Gen_Proc (I : in out Integer) is
   begin
      case In_Par is
         when 0 =>
            I := I + Ident_Int (2);
         when 10 =>
            I := I + Ident_Int (1);
         when -3_000 =>
            I := I + Ident_Int (3);
         when others =>
            I := I + Ident_Int (4);
      end case;

      case In_Out_Par is
         when 0 =>
            In_Out_Par := Ident_Int (0);
         when 100 =>
            In_Out_Par := Ident_Int (50);
         when -3_000 =>
            In_Out_Par := Ident_Int (-3_000);
         when others =>
            In_Out_Par := Ident_Int (5);
      end case;

      case (In_Par) is
         when 0 =>
            I := I + Ident_Int (2);
         when 10 =>
            I := I + Ident_Int (1);
         when -3_000 =>
            I := I + Ident_Int (3);
         when others =>
            I := I + Ident_Int (4);
      end case;

      case (In_Out_Par) is
         when 0 =>
            In_Out_Par := Ident_Int (200);
         when 50 =>
            In_Out_Par := Ident_Int (25);
         when -3_000 =>
            In_Out_Par := Ident_Int (300);
         when others =>
            In_Out_Par := Ident_Int (400);
      end case;

   end Gen_Proc;

   procedure P is new Gen_Proc (In_Var, In_Out_Var);

begin
   Test
     ("C54A13B",
      "CHECK THAT IF A CASE EXPRESSION IS A " &
      "GENERIC 'IN' OR 'IN OUT' PARAMETER WITH A " &
      "NON-STATIC SUBTYPE OR ONE OF " &
      "THESE IN PARENTHESES, THEN ANY VALUE OF " &
      "THE EXPRESSION'S BASE TYPE MAY APPEAR AS " &
      "A CHOICE");

   P (Check_Var);

   if not Equal (Check_Var, Ident_Int (3)) then
      Failed ("INCORRECT CHOICES MADE FOR IN PARAMETER IN CASE");
   end if;

   if not Equal (In_Out_Var, Ident_Int (25)) then
      Failed ("INCORRECT CHOICESMADE FOR IN OUT PARAMETER IN CASE");
   end if;

   Result;
end C54a13b;
