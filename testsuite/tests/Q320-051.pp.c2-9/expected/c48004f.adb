-- C48004F.ADA

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
-- CHECK THAT THE FORM "NEW T" IS PERMITTED IF T IS AN ACCESS TYPE.

-- RM  01/12/80
-- JBG 03/03/83
-- EG  07/05/84

with Report;

procedure C48004f is

   use Report;

begin

   Test
     ("C48004F",
      "CHECK THAT THE FORM 'NEW T' IS PERMITTED IF T " & "IS AN ACCESS TYPE");

   declare

      type Aint is access Integer;
      type A_Aint is access Aint;
      Va_Aint : A_Aint;

      type Ast is access String;
      subtype Cast_4 is Ast (1 .. 4);
      type A_Ast is access Ast;
      type Acast_3 is access Ast (1 .. 3);
      V_Aast    : A_Ast;
      V_Acast_3 : Acast_3;

      type Ur (A, B : Integer) is record
         C : Integer;
      end record;
      subtype Cr is Ur (1, 2);
      type A_Cr is access Cr;
      type Aa_Cr is access A_Cr;
      V_Aa_Cr : Aa_Cr;

   begin

      Va_Aint := new Aint;
      if Va_Aint.all /= null then
         Failed ("VARIABLE IS NOT NULL - CASE 1");
      end if;

      begin

         V_Acast_3 := new Cast_4;
         if V_Acast_3.all /= null then
            Failed ("VARIABLE IS NOT NULL - CASE 2");
         end if;

      exception

         when others =>
            Failed ("EXCEPTION RAISED - CASE 2");

      end;

      V_Aast := new Ast;
      if V_Aast.all /= null then
         Failed ("VARIABLE IS NOT NULL - CASE 3");
      end if;

      V_Aa_Cr := new A_Cr;
      if V_Aa_Cr.all /= null then
         Failed ("VARIABLE IS NOT NULL - CASE 4");
      end if;

   end;

   Result;

end C48004f;
