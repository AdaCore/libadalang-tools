-- C48004D.ADA

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
-- CHECK THAT THE FORM "NEW T" IS PERMITTED IF T IS A RECORD, PRIVATE, OR
-- LIMITED TYPE WITHOUT DISCRIMINANTS.

-- RM  01/12/80
-- JBG 03/03/83
-- EG  07/05/84

with Report;

procedure C48004d is

   use Report;

begin

   Test
     ("C48004D",
      "CHECK THAT THE FORM 'NEW T' IS PERMITTED IF T " &
      "IS A RECORD, PRIVATE, OR LIMITED TYPE WITHOUT " & "DISCRIMINANTS");

   declare

      type Tc is record
         C : Integer := 18;
      end record;
      type Atc is access Tc;
      Vc : Atc;

      package P is
         type Priv is private;
         type Lpriv is limited private;
         type A_Priv is access Priv;
         type A_Lpriv is access Lpriv;
         procedure Check (X : A_Priv);
         procedure Lcheck (X : A_Lpriv);
         procedure Lrcheck (X : Lpriv);
      private
         type Priv is record
            Q : Integer := 19;
         end record;
         type Lpriv is record
            Q : Integer := 20;
         end record;
      end P;

      Vp  : P.A_Priv;
      Vlp : P.A_Lpriv;

      type Lcr is record
         C : P.Lpriv;
      end record;
      type A_Lcr is access Lcr;
      Vlcr : A_Lcr;

      package body P is

         procedure Check (X : A_Priv) is
         begin
            if X.Q /= 19 then
               Failed ("WRONG VALUES - C2");
            end if;
         end Check;

         procedure Lcheck (X : A_Lpriv) is
         begin
            if X.Q /= 20 then
               Failed ("WRONG VALUES - C3");
            end if;
         end Lcheck;

         procedure Lrcheck (X : Lpriv) is
         begin
            if X.Q /= 20 then
               Failed ("WRONG VALUES - C4");
            end if;
         end Lrcheck;

      end P;

   begin

      Vc := new Tc;
      if Vc.C /= 18 then
         Failed ("WRONG VALUES  -  C1");
      end if;

      Vp := new P.Priv;
      P.Check (Vp);
      Vlp := new P.Lpriv;
      P.Lcheck (Vlp);

      Vlcr := new Lcr;
      P.Lrcheck (Vlcr.all.C);

   end;

   Result;

end C48004d;
