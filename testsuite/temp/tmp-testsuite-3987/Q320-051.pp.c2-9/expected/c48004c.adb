-- C48004C.ADA

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
-- CHECK THAT THE FORM "NEW T" IS PERMITTED IF T IS AN UNCONSTRAINED
-- RECORD, PRIVATE, OR LIMITED TYPE WHOSE DISCRIMINANTS HAVE DEFAULT
-- VALUES.

-- EG  08/03/84

with Report;

procedure C48004c is

   use Report;

begin

   Test
     ("C48004C",
      "CHECK THAT THE FORM 'NEW T' IS PERMITTED IF " &
      "T IS AN UNCONSTRAINED RECORD, PRIVATE, OR " &
      "LIMITED TYPE WHOSE DISCRIMINANTS HAVE DEFAULT " &
      "VALUES");

   declare

      type Ur (A : Integer := 1; B : Integer := 2) is record
         C : Integer := 7;
      end record;

      package P is

         type Up (A : Integer := 12; B : Integer := 13) is private;
         type Ul (A, B : Integer := 1) is limited private;

      private

         type Up (A : Integer := 12; B : Integer := 13) is record
            Q : Integer;
         end record;
         type Ul (A, B : Integer := 1) is record
            Q : Integer;
         end record;

      end P;

      use P;

      type A_Ur is access Ur;
      type A_Up is access Up;
      type A_Ul is access Ul;

      V_Ur : A_Ur;
      V_Up : A_Up;
      V_Ul : A_Ul;

   begin

      V_Ur := new Ur;
      if (V_Ur.A /= Ident_Int (1) or V_Ur.B /= 2 or V_Ur.C /= 7) then
         Failed ("WRONG VALUES - UR");
      end if;

      V_Up := new Up;
      if (V_Up.A /= Ident_Int (12) or V_Up.B /= 13) then
         Failed ("WRONG VALUES - UP");
      end if;

      V_Ul := new Ul;
      if (V_Ul.A /= Ident_Int (1) or V_Ul.B /= 1) then
         Failed ("WRONG VALUES - UL");
      end if;

   end;

   Result;

end C48004c;
