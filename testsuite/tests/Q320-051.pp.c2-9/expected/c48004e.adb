-- C48004E.ADA

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
-- CHECK THAT THE FORM "NEW T" IS PERMITTED IF T IS A CONSTRAINED ARRAY TYPE.

-- RM  01/12/80
-- JBG 03/03/83
-- EG  07/05/84

with Report;

procedure C48004e is

   use Report;

begin

   Test
     ("C48004E",
      "CHECK THAT THE FORM 'NEW T' IS PERMITTED IF T " &
      "IS A CONSTRAINED ARRAY TYPE");

   declare

      type Arr0 is array (Integer range <>) of Boolean;
      subtype Arr is Arr0 (1 .. 10);
      type A_Arr is access Arr;
      Varr : A_Arr;

      package P is
         type Lpriv is limited private;
         function Check (X : Lpriv) return Integer;
      private
         type Lpriv is record
            Q : Integer := 20;
         end record;
      end P;

      type Lparr is array (1 .. 2) of P.Lpriv;
      type A_Lparr is access Lparr;

      V_A_Lparr : A_Lparr;

      package body P is
         function Check (X : Lpriv) return Integer is
         begin
            return X.Q;
         end Check;
      end P;

   begin

      Varr := new Arr;
      if (Varr'First /= Ident_Int (1) or Varr'Last /= 10) then
         Failed ("WRONG BOUNDS - CASE 1");
      end if;

      V_A_Lparr := new Lparr;
      if
        (P.Check (V_A_Lparr.all (1)) /= Ident_Int (20) or
         P.Check (V_A_Lparr.all (2)) /= Ident_Int (20))
      then
         Failed ("WRONG VALUES - CASE 2");
      end if;

   end;

   Result;

end C48004e;
