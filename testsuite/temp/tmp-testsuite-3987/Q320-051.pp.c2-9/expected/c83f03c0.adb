-- C83F03C1.ADA

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
-- SEPARATELY COMPILED PACKAGE FOR USE WITH  C83F03C2M

-- THIS PACKAGE IS A FULL-FLEDGED COMPILATION UNIT (AS OPPOSED TO
--    BEING A SUBUNIT; SUBUNITS ARE TESTED IN  C83F03D0M ,
--    C83F03D1 ). THE PRESENT FILE CONTAINS THE BODY OF THE PACKAGE.

-- FOR THIS FILE, THE FILE NAME AND THE UNIT NAME ARE NOT THE SAME.

--    RM    05 SEPTEMBER 1980
-- PWN 11/30/94 ADDED A PROCEDURE TO KEEP PACKAGE BODIES LEGAL.

package body C83f03c0 is

   Y4 : Integer := 200;

   type T4 is (G, H, I);

   procedure Bump is
   begin
      Flow_Index := Flow_Index + 1;
   end Bump;

   procedure Require_Body is
   begin
      null;
   end Require_Body;

   package body P is
   begin

      goto T3;

      Bump;
      Bump;

      <<T3>>
      Bump;
      goto T4;
      Bump;
      <<LABEL_IN_OUTER>>
      Bump;
      goto Ending;
      Bump;
      <<Y3>>
      Bump;
      goto Y4;
      Bump;
      <<Y4>>
      Bump;
      goto Label_In_Outer;
      Bump;
      <<T4>>
      Bump;
      goto Y3;
      Bump;

      <<ENDING>>
      null;

   end P;

begin

   <<LABEL_IN_OUTER>>
   null;

end C83f03c0;
