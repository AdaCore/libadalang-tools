-- C95088A.ADA

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
-- CHECK THAT ACTUAL PARAMETERS ARE EVALUATED AND IDENTIFIED AT THE
--   TIME OF CALL.

-- GLH 7/10/85

with Report; use Report;

procedure C95088a is

   type Vector is array (1 .. 10) of Integer;
   type Ptrint is access Integer;

   I  : Integer := 1;
   A  : Vector  := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
   P1 : Ptrint  := new Integer'(2);
   P2 : Ptrint  := P1;

   task T1 is
      entry E1 (I : out Integer; J : out Integer);
   end T1;

   task body T1 is
   begin
      accept E1 (I : out Integer; J : out Integer) do
         I := 10;
         J := -1;
      end E1;
   end T1;

   task T2 is
      entry E2 (P : out Ptrint; I : out Integer);
   end T2;

   task body T2 is
   begin
      accept E2 (P : out Ptrint; I : out Integer) do
         P := new Integer'(3);
         I := 5;
      end E2;
   end T2;

begin

   Test
     ("C95088A",
      "CHECK THAT ACTUAL PARAMETERS ARE EVALUATED " &
      "AND IDENTIFIED AT THE TIME OF CALL");

   Comment ("FIRST CALL");
   T1.E1 (I, A (I));
   if (A /= (-1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) then
      Failed ("A(I) EVALUATED UPON RETURN");
   end if;

   Comment ("SECOND CALL");
   T2.E2 (P1, P1.all);
   if (P2.all /= 5) then
      Failed ("P1.ALL EVALUATED UPON RETURN");
   end if;

   Result;

end C95088a;
