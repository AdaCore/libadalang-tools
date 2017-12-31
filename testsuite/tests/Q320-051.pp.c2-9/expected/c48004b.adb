-- C48004B.ADA

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
-- CHECK THAT THE FORM "NEW T" IS PERMITTED IF T IS A CONSTRAINED RECORD,
-- PRIVATE, OR LIMITED PRIVATE TYPE.

-- RM  01/12/80
-- JBG 03/03/83
-- EG  07/05/84

with Report;

procedure C48004b is

   use Report;

begin

   Test
     ("C48004B",
      "CHECK THAT THE FORM 'NEW T' IS PERMITTED IF " &
      "T IS A CONSTRAINED RECORD, PRIVATE, OR " & "LIMITED PRIVATE TYPE");

   declare

      type Tb0 (A, B : Integer) is record
         C : Integer := 7;
      end record;
      subtype Tb is Tb0 (2, 3);
      type Atb is access Tb0;
      Vb : Atb;

      type Tbb0 (A, B : Integer := 5) is record
         C : Integer := 6;
      end record;
      subtype Tbb is Tbb0 (4, 5);
      type Atbb is access Tbb0;
      Vbb : Atbb;

      package P is
         type Priv0 (A, B : Integer) is private;
         type Lpriv0 (A, B : Integer := 1) is limited private;
         function Fun (Lp : Lpriv0) return Integer;
      private
         type Priv0 (A, B : Integer) is record
            Q : Integer;
         end record;
         type Lpriv0 (A, B : Integer := 1) is record
            Q : Integer := 7;
         end record;
      end P;

      use P;

      subtype Priv is P.Priv0 (12, 13);
      type A_Priv is access P.Priv0;
      Vp : A_Priv;

      type A_Lpriv is access Lpriv0;
      Vlp : A_Lpriv;

      type Lcr (A, B : Integer := 4) is record
         C : P.Lpriv0;
      end record;
      subtype Slcr is Lcr (1, 2);
      type A_Slcr is access Slcr;
      Vslcr : A_Slcr;

      package body P is
         function Fun (Lp : Lpriv0) return Integer is
         begin
            return Lp.Q;
         end Fun;
      end P;

   begin

      Vb := new Tb;
      if (Vb.A /= Ident_Int (2) or Vb.B /= 3 or Vb.C /= 7) then
         Failed ("WRONG VALUES  -  B1");
      end if;

      Vbb := new Tbb0;
      if (Vbb.A /= Ident_Int (5) or Vbb.B /= 5 or Vbb.C /= 6) then
         Failed ("WRONG VALUES  -  B2");
      end if;

      Vp := new Priv;
      if (Vp.A /= Ident_Int (12) or Vp.B /= 13) then
         Failed ("WRONG VALUES  -  B3");
      end if;

      Vlp := new Lpriv0;
      if
        (Vlp.A /= Ident_Int (1) or Vlp.B /= 1 or
         P.Fun (Vlp.all) /= Ident_Int (7))
      then
         Failed ("WRONG VALUES  -  B4");
      end if;

      Vslcr := new Slcr;
      if
        (Vslcr.A /= Ident_Int (1) or Vslcr.B /= Ident_Int (2) or
         P.Fun (Vslcr.C) /= Ident_Int (7))
      then
         Failed ("WRONG VALUES - B5");
      end if;

   end;

   Result;

end C48004b;
