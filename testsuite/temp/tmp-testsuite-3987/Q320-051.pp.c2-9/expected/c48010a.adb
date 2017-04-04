-- C48010A.ADA

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
-- CHECK THAT NULL ARRAYS AND NULL RECORDS CAN BE ALLOCATED.

-- EG  08/30/84

with Report;

procedure C48010a is

   use Report;

begin

   Test
     ("C48010A",
      "CHECK THAT NULL ARRAYS AND NULL RECORDS CAN " & "BE ALLOCATED");

   declare

      type Ca is array (4 .. 3) of Integer;
      type Cr is record
         null;
      end record;

      type A_Ca is access Ca;
      type A_Cr is access Cr;

      type Aa_Ca is access A_Ca;
      type Aa_Cr is access A_Cr;

      V_A_Ca  : A_Ca;
      V_A_Cr  : A_Cr;
      V_Aa_Ca : Aa_Ca;
      V_Aa_Cr : Aa_Cr;

   begin

      V_A_Ca := new Ca;
      if V_A_Ca = null then
         Failed ("NULL ARRAY WAS NOT ALLOCATED - CA");
      elsif V_A_Ca.all'First /= 4 and V_A_Ca.all'Last /= 3 then
         Failed ("NULL ARRAY BOUNDS ARE INCORRECT - CA");
      end if;

      V_A_Cr := new Cr;
      if V_A_Cr = null then
         Failed ("NULL RECORD WAS NOT ALLOCATED - CR");
      end if;

      V_Aa_Ca := new A_Ca'(new Ca);
      if V_Aa_Ca.all = null then
         Failed ("NULL ARRAY WAS NOT ALLOCATED - A_CA");
      elsif V_Aa_Ca.all.all'First /= 4 and V_Aa_Ca.all.all'Last /= 3 then
         Failed ("NULL ARRAY BOUNDS ARE INCORRECT - A_CA");
      end if;

      V_Aa_Cr := new A_Cr'(new Cr);
      if (V_Aa_Cr = null or V_Aa_Cr.all = null) then
         Failed ("NULL RECORD WAS NOT ALLOCATED - A_CR");
      end if;

   end;

   Result;

end C48010a;
