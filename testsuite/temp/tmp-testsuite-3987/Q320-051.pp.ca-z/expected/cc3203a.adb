-- CC3203A.ADA

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
-- CHECK THAT WHEN A GENERIC FORMAL LIMITED/NON LIMITED PRIVATE TYPE HAS
-- DISCRIMINANTS, THE ACTUAL PARAMETER CAN HAVE DEFAULT DISCRIMINANT
-- VALUES.

-- SPS 7/9/82

with Report; use Report;

procedure Cc3203a is
begin
   Test
     ("CC3203A",
      "CHECK DEFAULT VALUES FOR LIMITED/" &
      "NON LIMITED GENERIC FORMAL PRIVATE TYPES");
   declare
      Sd : Integer := Ident_Int (0);

      function Init_Rc (X : Integer) return Integer;

      type Rec (D : Integer := 3) is record
         null;
      end record;

      type Rc (C : Integer := Init_Rc (1)) is record
         null;
      end record;

      generic
         type Pv (X : Integer) is private;
         type Lp (X : Integer) is limited private;
      package Pack is
         subtype Npv is Pv;
         subtype Nlp is Lp;
      end Pack;

      function Init_Rc (X : Integer) return Integer is
      begin
         Sd := Sd + X;
         return Sd;
      end Init_Rc;

      package P1 is new Pack (Rec, Rc);

      package P2 is
         P1vp  : P1.Npv;
         P1vl  : P1.Nlp;
         P1vl2 : P1.Nlp;
      end P2;
      use P2;
   begin

      if P1vp.D /= Ident_Int (3) then
         Failed ("DEFAULT DISCRIMINANT VALUE WRONG");
      end if;

      if P1vl.C /= 1 then
         Failed ("DID NOT EVALUATE DEFAULT DISCRIMINANT");
      end if;

      if P1vl2.C /= Ident_Int (2) then
         Failed ("DID NOT EVALUATE DEFAULT DISCRIMINANT " & "WHEN NEEDED");
      end if;
   end;

   Result;

end Cc3203a;
