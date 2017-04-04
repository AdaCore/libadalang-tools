-- CC1227A.ADA

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
--     CHECK, WHEN DERIVING FROM A FORMAL TYPE, THAT ALL THE PREDEFINED
--     OPERATIONS ASSOCIATED WITH THE CLASS OF THE FORMAL TYPE ARE
--     DECLARED FOR THE DERIVED TYPE.

-- HISTORY:
--     BCB 04/04/88  CREATED ORIGINAL TEST.

with Report; use Report;
with System; use System;

procedure Cc1227a is

   generic
      type Form is range <>;
   package P is
      type Der_Form is new Form;
      function Ident_Der (X : Der_Form) return Der_Form;
      function Ident_Adr (Y : Address) return Address;
   end P;

   package body P is
      Der_Var                : Der_Form;
      Der_Form_Base_First    : Der_Form;
      Der_Form_First         : Der_Form;
      Der_Form_Last          : Der_Form;
      Der_Form_Size          : Der_Form;
      Der_Form_Width         : Der_Form;
      Der_Form_Pos           : Der_Form;
      Der_Form_Val           : Der_Form;
      Der_Form_Succ          : Der_Form;
      Der_Form_Pred          : Der_Form;
      Der_Form_Image         : String (1 .. 5);
      Der_Form_Value         : Der_Form;
      Der_Var_Size           : Der_Form;
      Der_Var_Address        : Address;
      Der_Equal, Der_Unequal : Der_Form;
      Der_Greater            : Der_Form;
      Der_Mod, Der_Rem       : Der_Form;
      Der_Abs, Der_Exp       : Der_Form;
      Int                    : Integer := 5;
      function Ident_Der (X : Der_Form) return Der_Form is
      begin
         if Equal (3, 3) then
            return X;
         end if;
         return 0;
      end Ident_Der;
      function Ident_Adr (Y : Address) return Address is
         X : Der_Form;
      begin
         if Equal (3, 3) then
            return Y;
         end if;
         return X'Address;
      end Ident_Adr;
   begin
      Test
        ("CC1227A",
         "CHECK, WHEN DERIVING FROM A FORMAL TYPE, " &
         "THAT ALL THE PREDEFINED OPERATIONS " &
         "ASSOCIATED WITH THE CLASS OF THE FORMAL " &
         "TYPE ARE DECLARED FOR THE DERIVED TYPE");

      Der_Var := Ident_Der (1);

      if Der_Var /= 1 then
         Failed ("IMPROPER VALUE FROM ASSIGNMENT OPERATION");
      end if;

      if Der_Var not in Der_Form then
         Failed ("IMPROPER RESULT FROM MEMBERSHIP TEST");
      end if;

      Der_Var := Der_Form'(2);

      if Der_Var /= Ident_Der (2) then
         Failed ("IMPROPER RESULT FROM QUALIFICATION");
      end if;

      Der_Var := Der_Form (Int);

      if Der_Var /= Ident_Der (5) then
         Failed ("IMPROPER RESULT FROM EXPLICIT CONVERSION - " & "INTEGER");
      end if;

      Der_Var := Der_Form (3.0);

      if Der_Var /= Ident_Der (3) then
         Failed ("IMPROPER RESULT FROM EXPLICIT CONVERSION - " & "FLOAT");
      end if;

      Der_Var := 1_000;

      if Der_Var /= Ident_Der (1_000) then
         Failed ("IMPROPER RESULT FROM IMPLICIT CONVERSION");
      end if;

      Der_Form_Base_First := Der_Form'Base'First;

      Der_Form_First := Der_Form'First;

      if Der_Form_Base_First /= Ident_Der (Der_Form_First) then
         Failed ("IMPROPER VALUE FOR DER_FORM'BASE'FIRST");
      end if;

      if Der_Form_First /= Ident_Der (Der_Form'First) then
         Failed ("IMPROPER VALUE FOR DER_FORM'FIRST");
      end if;

      Der_Form_Last := Der_Form'Last;

      if Der_Form_Last /= Ident_Der (Der_Form'Last) then
         Failed ("IMPROPER VALUE FOR DER_FORM'LAST");
      end if;

      Der_Form_Size := Der_Form (Der_Form'Size);

      if Der_Form_Size /= Ident_Der (Der_Form (Der_Form'Size)) then
         Failed ("IMPROPER VALUE FOR DER_FORM'SIZE");
      end if;

      Der_Form_Width := Der_Form (Der_Form'Width);

      if Der_Form_Width /= Ident_Der (Der_Form (Der_Form'Width)) then
         Failed ("IMPROPER VALUE FOR DER_FORM'WIDTH");
      end if;

      Der_Form_Pos := Der_Form (Der_Form'Pos (Der_Var));

      if Der_Form_Pos /= Ident_Der (Der_Form (Der_Form'Pos (Der_Var))) then
         Failed ("IMPROPER VALUE FOR DER_FORM'POS(DER_VAR)");
      end if;

      Der_Form_Val := Der_Form'Val (Der_Var);

      if Der_Form_Val /= Ident_Der (Der_Form'Val (Der_Var)) then
         Failed ("IMPROPER VALUE FOR DER_FORM'VAL(DER_VAR)");
      end if;

      Der_Form_Succ := Der_Form'Succ (Der_Var);

      if Der_Form_Succ /= Ident_Der (Der_Form'Succ (Der_Var)) then
         Failed ("IMPROPER VALUE FOR DER_FORM'SUCC(DER_VAR)");
      end if;

      Der_Form_Pred := Der_Form'Pred (Der_Var);

      if Der_Form_Pred /= Ident_Der (Der_Form'Pred (Der_Var)) then
         Failed ("IMPROPER VALUE FOR DER_FORM'PRED(DER_VAR)");
      end if;

      Der_Form_Image := Der_Form'Image (Der_Var);

      if Der_Form_Image (2 .. 5) /= "1000" then
         Failed ("IMPROPER VALUE FOR DER_FORM'IMAGE(DER_VAR)");
      end if;

      Der_Form_Value := Der_Form'Value (Der_Form_Image);

      if Der_Form_Value /= Ident_Der (1_000) then
         Failed ("IMPROPER VALUE FOR DER_FORM'VALUE" & "(DER_FORM_IMAGE)");
      end if;

      Der_Var_Size := Der_Form (Der_Var'Size);

      if Der_Var_Size /= Ident_Der (Der_Form (Der_Var'Size)) then
         Failed ("IMPROPER VALUE FOR DER_VAR'SIZE");
      end if;

      Der_Var_Address := Der_Var'Address;

      if Der_Var_Address /= Ident_Adr (Der_Var'Address) then
         Failed ("IMPROPER VALUE FOR DER_VAR'ADDRESS");
      end if;

      Der_Equal := Ident_Der (1_000);

      if Der_Var /= Der_Equal then
         Failed ("IMPROPER RESULT FROM INEQUALITY OPERATOR");
      end if;

      Der_Unequal := Ident_Der (500);

      if Der_Var = Der_Unequal then
         Failed ("IMPROPER RESULT FROM EQUALITY OPERATOR");
      end if;

      if Der_Var < Der_Unequal then
         Failed ("IMPROPER RESULT FROM LESS THAN OPERATOR");
      end if;

      if Der_Var <= Der_Unequal then
         Failed ("IMPROPER RESULT FROM LESS THAN OR EQUAL TO " & "OPERATOR");
      end if;

      Der_Greater := Ident_Der (1_500);

      if Der_Var > Der_Greater then
         Failed ("IMPROPER RESULT FROM GREATER THAN OPERATOR");
      end if;

      if Der_Var >= Der_Greater then
         Failed
           ("IMPROPER RESULT FROM GREATER THAN OR EQUAL " & "TO OPERATOR");
      end if;

      Der_Var := Der_Var + Der_Equal;

      if Der_Var /= Ident_Der (2_000) then
         Failed ("IMPROPER RESULT FROM ADDITION OPERATOR");
      end if;

      Der_Var := Der_Var - Der_Equal;

      if Der_Var /= Ident_Der (1_000) then
         Failed ("IMPROPER RESULT FROM SUBTRACTION OPERATOR");
      end if;

      Der_Var := Der_Var * Ident_Der (2);

      if Der_Var /= Ident_Der (2_000) then
         Failed ("IMPROPER RESULT FROM MULTIPLICATION OPERATOR");
      end if;

      Der_Var := Der_Var / Ident_Der (2);

      if Der_Var /= Ident_Der (1_000) then
         Failed ("IMPROPER RESULT FROM DIVISION OPERATOR");
      end if;

      Der_Mod := Der_Greater mod Der_Var;

      if Der_Mod /= Ident_Der (500) then
         Failed ("IMPROPER RESULT FROM MOD OPERATOR");
      end if;

      Der_Rem := Der_Greater rem Der_Var;

      if Der_Rem /= Ident_Der (500) then
         Failed ("IMPROPER RESULT FROM REM OPERATOR");
      end if;

      Der_Abs := abs (Ident_Der (-1_500));

      if Der_Abs /= Ident_Der (Der_Greater) then
         Failed ("IMPROPER RESULT FROM ABS OPERATOR");
      end if;

      Der_Exp := Ident_Der (2)**Ident_Int (2);

      if Der_Exp /= Ident_Der (4) then
         Failed ("IMPROPER RESULT FROM EXPONENTIATION OPERATOR");
      end if;

      Result;
   end P;

   package Pack is new P (Integer);

begin
   null;
end Cc1227a;
