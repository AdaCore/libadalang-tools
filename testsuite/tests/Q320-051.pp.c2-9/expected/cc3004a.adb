-- CC3004A.ADA

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
-- CHECK THAT ACTUAL PARAMETERS IN A NAMED GENERIC ACTUAL PARAMETER ASSOCIATION
-- MAY BE OUT OF ORDER, AND ARE ASSOCIATED WITH THE CORRECT FORMALS.

-- DAT 9/16/81
-- SPS 10/26/82

with Report; use Report;

procedure Cc3004a is
begin
   Test ("CC3004A", "ORDER OF NAMED GENERIC ACTUAL PARAMETERS");

   declare
      generic
         A, B : Integer;
         C : Integer;
         D : Integer;
      package P1 is
      end P1;

      type Ai is access Integer;

      generic
         type D is (<>);
         Vd : D;
         type Ad is access D;
         Va : Ad;
      package P2 is
      end P2;

      X : Ai := new Integer'(Ident_Int (23));
      Y : Ai := new Integer'(Ident_Int (77));

      package body P1 is
      begin
         if A /= Ident_Int (4) or
           B /= Ident_Int (12) or
           C /= Ident_Int (11) or
           D /= Ident_Int (-33)
         then
            Failed ("WRONG GENERIC PARAMETER ASSOCIATIONS");
         end if;
      end P1;

      package body P2 is
      begin
         if Va.all /= Vd then
            Failed ("WRONG GENERIC PARM ASSOCIATIONS 2");
         end if;
      end P2;

      package N1 is new P1 (C => 11, A => 4, D => -33, B => 12);

      package N2 is new P2 (Va => X, Ad => Ai, D => Integer, Vd => 23);

      package N3 is new P2 (Integer, 77, Va => Y, Ad => Ai);

   begin
      null;
   end;

   Result;
end Cc3004a;
