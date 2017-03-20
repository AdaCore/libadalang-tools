-- C2A008A.ADA

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
-- CHECK THAT UPPER AND LOWER CASE "E" MAY APPEAR IN BASED LITERALS,
-- WHEN USING COLONS IN PLACE OF THE SHARP SIGN.

-- TBN 2/28/86

with Report; use Report;
procedure C2a008a is

   type Float is digits 5;
   Int_1 : Integer := 15:A:E1;
   Int_2 : Integer := 15:A:e1;
   Flo_1 : Float   := 16:FD.C:E1;
   Flo_2 : Float   := 16:FD.C:e1;

begin
   Test
     ("C2A008A",
      "CHECK THAT UPPER AND LOWER CASE ""E"" MAY " &
      "APPEAR IN BASED LITERALS, WHEN USING COLONS " &
      "IN PLACE OF THE SHARP SIGN");

   if Int_1 /= Int_2 then
      Failed ("UPPER AND LOWER CASE LETTERS NOT ALLOWED - 1");
   end if;

   if Flo_1 /= Flo_2 then
      Failed ("UPPER AND LOWER CASE LETTERS NOT ALLOWED - 2");
   end if;

   Int_1 := 14:BC:E1;
   Int_2 := 14:BC:e1;
   Flo_1 := 16:DEF.AB:E0;
   Flo_2 := 16:DEF.AB:e0;

   if Int_1 /= Int_2 then
      Failed ("UPPER AND LOWER CASE LETTERS NOT ALLOWED - 3");
   end if;

   if Flo_1 /= Flo_2 then
      Failed ("UPPER AND LOWER CASE LETTERS NOT ALLOWED - 4");
   end if;

   Result;
end C2a008a;
